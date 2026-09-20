# The code generator is the single source of truth for what a figure looks
# like. `gs_code_plot()` emits the ggplot2 expression as text; the preview, the
# "Export all figures" button and the R-code tab all draw their figure by
# evaluating exactly that text. A figure can therefore never drift from the
# code the user is told to run.
#
# The claim is about the figure, and stops where the figure does. Getting to
# `d` -- the categorized columns, the rows dropped for a missing layer value,
# the subset that makes this figure its stratum -- is written here too, by
# gs_code_script(), but the app reaches `d` by calling the helpers rather than
# by evaluating those lines. The two stay in step because they share the one
# implementation underneath: gs_apply_cuts() runs the very lines
# gs_code_cut_line() writes, and gs_split_strata() takes the subset
# gs_code_script() prints. gs_eval_plot() is handed a `d` that is already
# there, and evaluates only the figure.

# --- literal helpers ---------------------------------------------------------

# Reserved words plus the handful of names that would be confusing unquoted.
GS_RESERVED <- c("if", "else", "repeat", "while", "function", "for", "next",
                 "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
                 "NA_integer_", "NA_real_", "NA_character_", "in")

#' Quote a column name for use in generated code
#' @keywords internal
#' @noRd
gs_bt <- function(x) {
  ok <- grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x) &
    !grepl("^\\.[0-9]", x) &
    !x %in% GS_RESERVED
  ifelse(ok, x, paste0("`", gsub("`", "\\\\`", x), "`"))
}

#' Quote a string literal for use in generated code
#' @keywords internal
#' @noRd
gs_dq <- function(x) encodeString(as.character(x), quote = "\"")

#' Format a number without scientific notation
#' @keywords internal
#' @noRd
gs_n <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("NA")
  if (x == round(x) && abs(x) < 1e15) return(format(as.integer(round(x))))
  format(x, scientific = FALSE, trim = TRUE)
}

#' Indent every line of a character vector, leaving blank lines blank
#' @keywords internal
#' @noRd
gs_indent <- function(lines, prefix = "  ") {
  ifelse(nzchar(lines), paste0(prefix, lines), lines)
}

# --- plot semantics ----------------------------------------------------------

#' Which aesthetic a "Group (or colour)" selection maps to for a plot type
#' @keywords internal
#' @noRd
gs_group_aes <- function(plot_type) {
  if (plot_type %in% c("Scatter", GS_LINE, "Dot + Error", GS_KM)) "colour" else "fill"
}

# --- panel strips ------------------------------------------------------------

# The column add_facet_n() writes. Dot-prefixed, and kept out of the data by
# gs_clean_names(), so that it cannot collide with a column of the user's.
GS_FACET_COL <- ".facet_label"

# Emitted once, and only when the panels are asked to carry their size. Written
# out as text rather than exported so that the generated code stands on its own.
GS_FACET_N_HELPER <- c(
  "# A panel that does not say how many observations it holds invites the",
  "# reader to compare shapes drawn from 300 rows and from 8, and a strip",
  "# reading \"[18,53]\" does not say what the range is a range of.",
  "# add_facet_n() writes the variable and the size into the strip label,",
  "# keeping the level order intact so that the panels stay in the order the",
  "# factor declares.",
  "add_facet_n <- function(d, var) {",
  "  d <- data.table::copy(data.table::as.data.table(d))",
  "  v <- as.character(d[[var]])",
  "  lv <- if (is.factor(d[[var]])) levels(d[[var]]) else sort(unique(v))",
  "  sizes <- as.integer(table(factor(v, levels = lv)))",
  "  strips <- sprintf(\"%s: %s (N = %d)\", var, lv, sizes)",
  sprintf("  d[, %s := factor(strips[match(v, lv)], levels = strips)]",
          GS_FACET_COL),
  "  d[]",
  "}",
  ""
)

# The same helper for a weighted figure. A separate text rather than an
# optional argument, so that an unweighted figure's code is unchanged.
GS_FACET_N_WEIGHTED_HELPER <- c(
  "# A panel that does not say how many observations it holds invites the",
  "# reader to compare shapes drawn from 300 rows and from 8. With a survey",
  "# weight the strip carries two counts: the rows the panel is drawn from,",
  "# and the number of people they stand for, which is the sum of their",
  "# weights. add_facet_n() writes both into the strip label, keeping the level",
  "# order intact so that the panels stay in the order the factor declares.",
  "add_facet_n <- function(d, var, weight) {",
  "  d <- data.table::copy(data.table::as.data.table(d))",
  "  v <- as.character(d[[var]])",
  "  lv <- if (is.factor(d[[var]])) levels(d[[var]]) else sort(unique(v))",
  "  f <- factor(v, levels = lv)",
  "  sizes <- as.integer(table(f))",
  "  wsums <- vapply(split(as.numeric(d[[weight]]), f), sum, numeric(1))",
  "  strips <- sprintf(\"%s: %s (N = %d; weighted N = %s)\", var, lv, sizes,",
  "                    format(round(wsums), big.mark = \",\",",
  "                           scientific = FALSE, trim = TRUE))",
  sprintf("  d[, %s := factor(strips[match(v, lv)], levels = strips)]",
          GS_FACET_COL),
  "  d[]",
  "}",
  ""
)

#' Does this figure label its panels with their size?
#'
#' Only a facet variable of the user's is labelled here. The all-figures
#' preview builds its own `.strat_label`, which already carries the size.
#' @keywords internal
#' @noRd
gs_uses_facet_n <- function(spec, facet_strata = FALSE) {
  !isTRUE(facet_strata) && nzchar(spec$facet) && isTRUE(spec$show_n)
}

#' The column the figure is panelled by, which is not always a real column
#' @keywords internal
#' @noRd
gs_facet_col <- function(spec, facet_strata = FALSE) {
  if (isTRUE(facet_strata)) return(".strat_label")
  if (!nzchar(spec$facet)) return("")
  if (isTRUE(spec$show_n)) GS_FACET_COL else spec$facet
}

# --- categorized variables ---------------------------------------------------

#' The expression for one categorization rule
#'
#' `"missing"` is the one method that does not read the column's values. It
#' asks only whether there is a value, which is a question any type answers,
#' and it answers it for every row: `is.na()` is never itself `NA`, so the
#' derived column never costs a row at the layer stage.
#'
#' The labels are spelled out rather than left as `FALSE`/`TRUE` because they
#' become axis text, strip text and file names.
#' @keywords internal
#' @noRd
gs_cut_expr <- function(cut) {
  v <- gs_bt(cut$var)
  switch(
    cut$method,
    period   = gs_time_expr(cut, v),
    quantile = sprintf(paste0("cut(%s, breaks = unique(stats::quantile(%s, ",
                              "probs = seq(0, 1, length.out = %s), na.rm = TRUE)), ",
                              "include.lowest = TRUE)"),
                       v, v, gs_n(cut$n + 1L)),
    equal    = sprintf("cut(%s, breaks = %s)", v, gs_n(cut$n)),
    breaks   = sprintf("cut(%s, breaks = c(-Inf, %s, Inf))", v,
                       paste(gs_n_vec(cut$breaks), collapse = ", ")),
    missing  = sprintf(paste0("factor(is.na(%s), levels = c(FALSE, TRUE), ",
                              "labels = c(%s, %s))"),
                       v, gs_dq(GS_OBSERVED_LABEL), gs_dq(GS_MISSING_LABEL)),
    stop("Unknown cut method: ", cut$method, call. = FALSE)
  )
}

#' The expression for one time resolution
#'
#' Two families, and the difference between them is the point of the control.
#'
#' A *calendar period* floors the column to the start of the period and stays a
#' date. That is deliberate: a year written as `2021-01-01` rather than as the
#' factor level `"2021"` keeps the column on a date scale, so a trend drawn
#' against it is spaced by real elapsed time and the axis can still be given
#' date ticks.
#'
#' Down to the day that is `cut()`, which floors a `Date` and a `POSIXct`
#' alike; `as.IDate()` on the way in makes the one case where `cut()` hands
#' back a factor rather than a date irrelevant, and honours the column's own
#' time zone while it converts.
#'
#' The hour and the minute are floored by arithmetic instead, and not for
#' brevity. `as.POSIXct(cut(x, "hour"))` looks right and is not: `cut()`
#' formats its labels in the column's time zone, `as.POSIXct()` reads them back
#' in the session's, and the instant moves by the offset between the two -- six
#' hours, silently, for a UTC column read in US Central. Subtracting the
#' remainder never leaves the epoch, so the class and the `tzone` survive
#' untouched.
#'
#' A *position in the cycle* throws the year away and pools every March
#' together. Those come out as a factor whose levels are written down in full,
#' in calendar order -- January to December, Monday to Sunday, spring to winter
#' -- because the alternative is `sort()`, and sorting the months of the year
#' alphabetically puts April first and is never what anyone meant.
#' @keywords internal
#' @noRd
gs_time_expr <- function(cut, v) {
  switch(
    cut$unit,
    # -- calendar period --
    year    = sprintf('as.IDate(cut(as.IDate(%s), breaks = "year"))', v),
    quarter = sprintf('as.IDate(cut(as.IDate(%s), breaks = "quarter"))', v),
    month   = sprintf('as.IDate(cut(as.IDate(%s), breaks = "month"))', v),
    week    = sprintf('as.IDate(cut(as.IDate(%s), breaks = "week"))', v),
    day     = sprintf("as.IDate(%s)", v),
    hour    = sprintf("%s - as.numeric(%s) %%%% 3600", v, v),
    minute  = sprintf("%s - as.numeric(%s) %%%% 60", v, v),
    # -- position in the cycle --
    month_of_year = sprintf(
      "factor(month.abb[month(%s)], levels = month.abb)", v),
    season = sprintf(
      paste0("factor(%s[((month(%s) - %dL) %%%% 12L) %%/%% 3L + 1L], ",
             "levels = %s)"),
      gs_chr_vec(GS_SEASONS), v, cut$season_start, gs_chr_vec(GS_SEASONS)),
    quarter_of_year = sprintf(
      'factor(paste0("Q", quarter(%s)), levels = paste0("Q", 1:4))', v),
    # data.table's wday() counts from Sunday, so the lookup is Sunday-first
    # while the levels are Monday-first: the week is read out in the order it
    # is worked, not in the order the function happens to number it.
    day_of_week = sprintf(
      paste0("factor(%s[wday(%s)], levels = %s)"),
      gs_chr_vec(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")), v,
      gs_chr_vec(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))),
    # Zero-padded, so that the hours sort and read as a clock rather than
    # putting 10 next to 1.
    hour_of_day = sprintf(
      'factor(sprintf("%%02d", hour(%s)), levels = sprintf("%%02d", 0:23))', v),
    stop("Unknown time resolution: ", cut$unit, call. = FALSE)
  )
}

#' A character vector as the `c("a", "b")` literal that builds it
#' @keywords internal
#' @noRd
gs_chr_vec <- function(x) {
  sprintf("c(%s)", paste(vapply(x, gs_dq, character(1L)), collapse = ", "))
}

#' The single `data.table` line that adds one derived column
#' @keywords internal
#' @noRd
gs_code_cut_line <- function(cut, data_sym = "dt") {
  sprintf("%s[, %s := %s]", data_sym, gs_bt(cut$new), gs_cut_expr(cut))
}

#' The block that adds every derived column, or `character(0)`
#'
#' Emitted once, straight after the data is loaded, so that a derived variable
#' can be used anywhere a real column can -- including as a stratifying
#' variable.
#' @keywords internal
#' @noRd
gs_code_cuts <- function(cuts, data_sym = "dt") {
  cuts <- gs_as_cuts(cuts)
  if (!length(cuts)) return(character())
  c("# Derived variables.",
    vapply(cuts, gs_code_cut_line, character(1L), data_sym = data_sym),
    "")
}

# --- the layers --------------------------------------------------------------

#' The block that excludes rows with no value for a layer variable
#'
#' A row that does not say which panel or which figure it belongs to cannot be
#' drawn in one. Dropping it quietly would change every denominator on the
#' screen without saying so, so the count is reported as the script runs.
#' @keywords internal
#' @noRd
gs_code_layer_na <- function(spec, data_sym = "dt") {
  # A design-based figure has already set aside the rows the design cannot
  # hold, in gs_code_design(); every other figure sets them aside here.
  vars <- if (gs_uses_design(spec)) gs_layer_vars(spec) else gs_exclude_vars(spec)
  if (!length(vars)) return(character())
  c("# Rows with no value for a layer variable cannot be placed in a panel or",
    "# a figure, so they are excluded here -- once, before anything is counted.",
    if (length(intersect(vars, gs_design_vars(spec)))) {
      c("# A row with no survey weight, sampling stratum or cluster cannot be",
        "# counted as part of the survey, and is excluded in the same way.")
    },
    gs_code_na_loop(vars, data_sym, "layer_vars"))
}

#' The loop that drops, and reports, the rows missing any of `vars`
#' @keywords internal
#' @noRd
gs_code_na_loop <- function(vars, data_sym, name) {
  c(sprintf("%s <- c(%s)", name, paste(gs_dq(vars), collapse = ", ")),
    sprintf("for (v in %s) {", name),
    sprintf("  n_na <- sum(is.na(%s[[v]]))", data_sym),
    "  if (n_na > 0L) {",
    "    message(\"Excluded \", n_na, \" row(s) with a missing \", v, \".\")",
    "  }",
    sprintf("  %s <- %s[!is.na(%s[[v]])]", data_sym, data_sym, data_sym),
    "}",
    "")
}

# --- the survey design -------------------------------------------------------

#' The block that builds the survey design, or `character(0)`
#'
#' Emitted before the layer rows are excluded and before the data is subset to
#' one figure, because the design is the whole sample's: see
#' `gs_design_data()`.
#' @keywords internal
#' @noRd
gs_code_design <- function(spec, data_sym = "dt") {
  if (!gs_uses_design(spec)) return(character())
  c("# The survey design, built over every row with a value for each design",
    "# variable, before any row is set aside for a layer. A panel or a figure",
    "# is a subpopulation of this design, so its standard errors come from",
    "# every stratum and cluster in the sample rather than from a design",
    "# rebuilt on the rows it happens to hold. .svy_row is each row's place in",
    "# the design, which is how a figure's rows find themselves in it.",
    gs_code_na_loop(gs_design_vars(spec), data_sym, "design_vars"),
    gs_code_design_call(spec, data_sym),
    "")
}

#' The two lines that number the rows and build the design from them
#'
#' Also what the app evaluates to build its own design, see
#' `gs_build_design()`.
#' @keywords internal
#' @noRd
gs_code_design_call <- function(spec, data_sym = "dt") {
  strata <- spec$design_strata %||% ""
  cluster <- spec$design_cluster %||% ""
  args <- c(
    sprintf("ids = ~%s", if (nzchar(cluster)) gs_bt(cluster) else "1"),
    if (nzchar(strata)) sprintf("strata = ~%s", gs_bt(strata)),
    sprintf("weights = ~%s", gs_bt(spec$weight)),
    # A cluster ID is read within its stratum: cluster 1 of one stratum is not
    # cluster 1 of the next, which is how most surveys number them.
    if (nzchar(strata) && nzchar(cluster)) "nest = TRUE")
  c(sprintf("%s[, .svy_row := .I]", data_sym),
    sprintf("des <- survey::svydesign(%s,", paste(args, collapse = ", ")),
    sprintf("                         data = as.data.frame(%s))", data_sym))
}

# --- Kaplan-Meier ------------------------------------------------------------

# Column names km_data() and km_risk() produce. Dot-prefixed so that they
# cannot collide with a grouping variable the user is fitting by;
# gs_clean_names() keeps the data free of them for the same reason.
GS_KM_COLS <- c(".time", ".surv", ".lower", ".upper", ".ncens", ".nrisk")

# The columns the survey-weighted helpers produce or work in, reserved for the
# same reason.
GS_SVY_COLS <- c(".y", ".ymin", ".ymax", ".w", ".svy_row", ".nrisk_label")

# The helper the generated script defines once and the loop then calls. It is
# emitted as text rather than exported so that the script stands on its own:
# running it needs survival and data.table, not ggstratify.
GS_KM_HELPER <- c(
  "# survfit() returns one row per event time. km_data() turns a fit into the",
  "# data frame the step plot is drawn from, one curve per level of `by`, and",
  "# starts every curve at (0, 1). The time and event columns are copied to",
  "# .t and .e first, so that a variable named `time` in the data cannot be",
  "# confused with this function's own argument.",
  "km_data <- function(d, time, event, by = character()) {",
  "  one <- function(tt, ev) {",
  "    f <- survival::survfit(survival::Surv(tt, ev) ~ 1)",
  "    na <- rep(NA_real_, length(f$time))",
  "    list(.time  = c(0, f$time),",
  "         .surv  = c(1, f$surv),",
  "         .lower = c(1, if (is.null(f$lower)) na else f$lower),",
  "         .upper = c(1, if (is.null(f$upper)) na else f$upper),",
  "         .ncens = c(0L, f$n.censor))",
  "  }",
  "  cols <- unique(c(time, event, by))",
  "  d <- data.table::as.data.table(d)[, ..cols]",
  "  .ok <- !is.na(d[[time]]) & !is.na(d[[event]])",
  "  d <- d[.ok]",
  "  d[[\".t\"]] <- as.numeric(d[[time]])",
  "  d[[\".e\"]] <- as.numeric(d[[event]])",
  "  if (length(by)) d[, one(.t, .e), by = by] else d[, one(.t, .e)]",
  "}",
  ""
)

# The second helper, defined only when the number-at-risk table is asked for.
# It answers a different question from km_data(): not "what is the estimate at
# this time" but "how many people were still being followed", which is what
# tells a reader whether the tail of the curve is worth reading at all.
GS_KM_RISK_HELPER <- c(
  "# summary(fit, times =) reports the number still at risk at each requested",
  "# time. extend = TRUE keeps a time beyond the last event in the table, as a",
  "# count of zero, rather than dropping the column out of the figure.",
  "km_risk <- function(d, time, event, times, by = character()) {",
  "  one <- function(tt, ev) {",
  "    f <- survival::survfit(survival::Surv(tt, ev) ~ 1)",
  "    s <- summary(f, times = times, extend = TRUE)",
  "    list(.time = times, .nrisk = as.integer(s$n.risk))",
  "  }",
  "  cols <- unique(c(time, event, by))",
  "  d <- data.table::as.data.table(d)[, ..cols]",
  "  .ok <- !is.na(d[[time]]) & !is.na(d[[event]])",
  "  d <- d[.ok]",
  "  d[[\".t\"]] <- as.numeric(d[[time]])",
  "  d[[\".e\"]] <- as.numeric(d[[event]])",
  "  if (length(by)) d[, one(.t, .e), by = by] else d[, one(.t, .e)]",
  "}",
  ""
)

# The survey-weighted counterparts of the two helpers above, emitted instead of
# them when a weight is set.
GS_KM_WEIGHTED_HELPER <- c(
  "# The survey-weighted counterpart of km_data(). survey::svykm() estimates",
  "# each curve from `design`, the survey design built over every row of the",
  "# data, as a subpopulation of it: the rows of `d` in that curve's group,",
  "# found in the design by .svy_row. When se = TRUE it also estimates the",
  "# design-based variance of the curve's log, strata and clusters included,",
  "# from which the 95% band is drawn the way confint() on an svykm fit draws",
  "# it. svykm() reports the event times only, so the censoring times are",
  "# added as flat steps of the same curve, and counted for the censoring",
  "# marks. Its estimate is not survfit()'s: even with every weight equal to",
  "# one the two differ slightly, most in the tail.",
  "km_data_weighted <- function(d, design, time, event, by = character(),",
  "                             se = FALSE) {",
  "  v <- design$variables",
  "  design$variables[[\".t\"]] <- as.numeric(v[[time]])",
  "  # Surv() reads 0/1, 1/2 and TRUE/FALSE alike; its status column is 0/1.",
  "  design$variables[[\".e\"]] <- survival::Surv(as.numeric(v[[time]]),",
  "    as.numeric(v[[event]]))[, \"status\"]",
  "  cols <- unique(c(\".svy_row\", time, event, by))",
  "  d <- data.table::as.data.table(d)[, ..cols]",
  "  .ok <- !is.na(d[[time]]) & !is.na(d[[event]])",
  "  d <- d[.ok]",
  "  one <- function(rows) {",
  "    tt <- design$variables[[\".t\"]][rows]",
  "    ev <- design$variables[[\".e\"]][rows]",
  "    times <- sort(unique(tt))",
  "    # A curve with no event on it stays at 1, and so does its band.",
  "    surv <- rep(1, length(times))",
  "    lower <- upper <- rep(if (se) 1 else NA_real_, length(times))",
  "    if (any(ev == 1)) {",
  "      f <- survey::svykm(survival::Surv(.t, .e) ~ 1, design[rows, ], se = se)",
  "      at <- findInterval(times, f$time)",
  "      surv <- c(1, f$surv)[at + 1L]",
  "      if (se) {",
  "        half <- stats::qnorm(0.975) * sqrt(c(0, f$varlog)[at + 1L])",
  "        lower <- surv * exp(-half)",
  "        upper <- pmin(surv * exp(half), 1)",
  "      }",
  "    }",
  "    list(.time  = c(0, times),",
  "         .surv  = c(1, surv),",
  "         .lower = c(1, lower),",
  "         .upper = c(1, upper),",
  "         .ncens = c(0L, tabulate(match(tt[ev == 0], times), length(times))))",
  "  }",
  "  if (length(by)) d[, one(.svy_row), by = by] else d[, one(.svy_row)]",
  "}",
  ""
)

GS_KM_RISK_WEIGHTED_HELPER <- c(
  "# The number still at risk at each requested time, counted twice: the rows",
  "# still being followed, and the people they stand for, which is the sum of",
  "# their weights. Both go into one label, the weighted count in parentheses.",
  "km_risk_weighted <- function(d, time, event, weight, times, by = character()) {",
  "  one <- function(tt, w) {",
  "    n <- vapply(times, function(s) sum(tt >= s), integer(1))",
  "    n_w <- vapply(times, function(s) sum(w[tt >= s]), numeric(1))",
  "    list(.time = times, .nrisk = n,",
  "         .nrisk_label = sprintf(\"%d\\n(%s)\", n, format(round(n_w),",
  "           big.mark = \",\", scientific = FALSE, trim = TRUE)))",
  "  }",
  "  cols <- unique(c(time, event, weight, by))",
  "  d <- data.table::as.data.table(d)[, ..cols]",
  "  .ok <- !is.na(d[[time]]) & !is.na(d[[event]])",
  "  d <- d[.ok]",
  "  d[[\".t\"]] <- as.numeric(d[[time]])",
  "  d[[\".w\"]] <- as.numeric(d[[weight]])",
  "  if (length(by)) d[, one(.t, .w), by = by] else d[, one(.t, .w)]",
  "}",
  ""
)

# The error-bar helpers. Emitted as text, like the Kaplan-Meier ones, so that
# the printed script stands on its own: running it needs nothing that is not
# already attached.
#
# Each returns the three columns stat_summary() draws a pointrange from -- the
# point at `y`, the ends at `ymin` and `ymax` -- and each is handed one
# stratum's values at a time.

GS_ERR_CI_HELPER <- c(
  "# A mean and its confidence interval. The t distribution rather than the",
  "# normal one: the standard error is itself estimated from the same n",
  "# observations, and t is what accounts for that. This is what",
  "# Hmisc::smean.cl.normal computes, written out so that the script needs no",
  "# package beyond the ones it already loads.",
  "mean_ci <- function(x, conf = 0.95) {",
  "  x <- x[!is.na(x)]",
  "  n <- length(x)",
  "  m <- if (n) mean(x) else NA_real_",
  "  # One observation has no spread to estimate from, so it gets a point and",
  "  # no interval rather than a NaN half-width.",
  "  if (n < 2L) return(data.frame(y = m, ymin = NA_real_, ymax = NA_real_))",
  "  half <- qt(1 - (1 - conf) / 2, n - 1L) * sd(x) / sqrt(n)",
  "  data.frame(y = m, ymin = m - half, ymax = m + half)",
  "}",
  ""
)

GS_ERR_EXACT_HELPER <- c(
  "# A proportion and its exact (Clopper-Pearson) interval. binom.test()",
  "# inverts the binomial test itself, so the interval covers at least conf of",
  "# the time at every n and every p. That is what a Wald interval on a",
  "# proportion does not do: near 0 or 1 it runs outside the range a",
  "# proportion can take, and when nobody had the outcome it has zero width",
  "# and claims certainty.",
  "prop_ci_exact <- function(x, conf = 0.95) {",
  "  x <- as.numeric(x)",
  "  x <- x[!is.na(x)]",
  "  n <- length(x)",
  "  if (!n) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))",
  "  ci <- binom.test(sum(x), n, conf.level = conf)$conf.int",
  "  data.frame(y = mean(x), ymin = ci[1], ymax = ci[2])",
  "}",
  ""
)

GS_ERR_WILSON_HELPER <- c(
  "# A proportion and its Wilson score interval: the values of p that the",
  "# score test does not reject. It stays inside 0 and 1, and it keeps a real",
  "# width when nobody or everybody had the outcome. Identical to",
  "# prop.test(correct = FALSE)$conf.int.",
  "prop_ci_wilson <- function(x, conf = 0.95) {",
  "  x <- as.numeric(x)",
  "  x <- x[!is.na(x)]",
  "  n <- length(x)",
  "  if (!n) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))",
  "  p <- mean(x)",
  "  z <- qnorm(1 - (1 - conf) / 2)",
  "  denom <- 1 + z^2 / n",
  "  centre <- (p + z^2 / (2 * n)) / denom",
  "  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom",
  "  # Inside [0, 1] by construction; the clamp only keeps floating-point dust",
  "  # from printing as -2.8e-17.",
  "  data.frame(y = p, ymin = max(0, centre - half), ymax = min(1, centre + half))",
  "}",
  ""
)

# A weighted Dot + Error figure cannot go through stat_summary(), which hands
# its function the Y values and nothing else -- not the weights. The points
# and their bars are estimated first, the way a Kaplan-Meier curve is, and the
# figure is drawn from the estimates.
GS_SVY_SUMMARY_HELPER <- c(
  "# Survey-weighted points and bars for a Dot + Error figure, one row per",
  "# point. `design` is the survey design built over every row of the data,",
  "# and each point is a subpopulation of it -- the rows of `d` in that",
  "# point's group, found in the design by .svy_row -- so its standard error",
  "# comes from the whole design, strata and clusters included, rather than",
  "# from a design rebuilt on the rows the group holds. Intervals are read on",
  "# the whole design's degrees of freedom, as for any subpopulation.",
  "#   bar = \"se\"      weighted mean, one standard error either side",
  "#   bar = \"normal\"  weighted mean, t interval",
  "#   bar = \"exact\"   svyciprop(method = \"beta\"): Korn and Graubard's",
  "#                   survey counterpart of Clopper-Pearson",
  "#   bar = \"wilson\"  svyciprop(method = \"wilson\"): the score interval",
  "# At a proportion of exactly 0 or 1 the design variance is zero and",
  "# svyciprop() has no effective sample size to work from, so the same",
  "# interval is computed on Kish's effective sample size instead.",
  "svy_summary <- function(d, design, y, by = character(), bar = \"se\",",
  "                        conf = 0.95, event = NULL) {",
  "  v <- design$variables[[y]]",
  "  design$variables[[\".y\"]] <- if (is.null(event)) as.numeric(v) else",
  "    as.numeric(as.character(v) == event)",
  "  dof <- survey::degf(design)",
  "  cols <- unique(c(\".svy_row\", y, by))",
  "  d <- data.table::as.data.table(d)[, ..cols]",
  "  .ok <- !is.na(d[[y]])",
  "  d <- d[.ok]",
  "  one <- function(rows) {",
  "    part <- design[rows, ]",
  "    m <- survey::svymean(~.y, part)",
  "    p <- unname(coef(m))",
  "    # One observation has no spread to estimate from, and a group whose",
  "    # weights are all zero has no mean.",
  "    if (length(rows) < 2L || is.na(p)) {",
  "      return(list(.y = p, .ymin = NA_real_, .ymax = NA_real_))",
  "    }",
  "    ci <- if (bar == \"se\") {",
  "      p + c(-1, 1) * as.numeric(survey::SE(m))",
  "    } else if (bar == \"normal\") {",
  "      confint(m, level = conf, df = dof)",
  "    } else if (p > 0 && p < 1) {",
  "      confint(survey::svyciprop(~.y, part, level = conf, df = dof,",
  "        method = if (bar == \"exact\") \"beta\" else \"wilson\"))",
  "    } else {",
  "      w <- weights(part)",
  "      n <- sum(w)^2 / sum(w^2)",
  "      a <- 1 - conf",
  "      if (bar == \"exact\") {",
  "        c(if (p > 0) qbeta(a / 2, n * p, n * (1 - p) + 1) else 0,",
  "          if (p < 1) qbeta(1 - a / 2, n * p + 1, n * (1 - p)) else 1)",
  "      } else {",
  "        z <- qnorm(1 - a / 2)",
  "        centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)",
  "        half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)",
  "        c(max(0, centre - half), min(1, centre + half))",
  "      }",
  "    }",
  "    list(.y = p, .ymin = unname(ci[1]), .ymax = unname(ci[2]))",
  "  }",
  "  # keyby rather than by, so the estimates read in the order of the levels.",
  "  if (length(by)) d[, one(.svy_row), keyby = by] else d[, one(.svy_row)]",
  "}",
  ""
)

#' The variables a weighted Dot + Error figure is estimated separately within
#'
#' One point per x value, per colour and per panel: everything that separates
#' one point from another has to separate its estimate too.
#' @keywords internal
#' @noRd
gs_dot_by <- function(spec, facet_strata = FALSE) {
  by <- c(spec$x, spec$group, gs_facet_col(spec, facet_strata))
  unique(by[nzchar(by)])
}

#' The helper one error-bar setting needs defined, or `character(0)`
#' @keywords internal
#' @noRd
gs_err_helper <- function(err_type) {
  switch(err_type,
         normal = GS_ERR_CI_HELPER,
         exact  = GS_ERR_EXACT_HELPER,
         wilson = GS_ERR_WILSON_HELPER,
         character())
}

#' What stat_summary() computes each Dot + Error point from
#'
#' `mean_se` is ggplot2's own, so the default setting emits exactly the line it
#' always did and needs nothing defined above it.
#' @keywords internal
#' @noRd
gs_dot_stat_args <- function(spec) {
  if (identical(spec$err_type, "se")) return("fun.data = mean_se")
  fun <- switch(spec$err_type,
                normal = "mean_ci",
                exact  = "prop_ci_exact",
                wilson = "prop_ci_wilson",
                stop("Unknown error bar: ", spec$err_type, call. = FALSE))
  sprintf("fun.data = %s, fun.args = list(conf = %s)", fun,
          gs_n(spec$err_level))
}

#' The layers a Dot + Error figure is drawn from, in drawing order
#'
#' The connecting line is drawn first, so that the points and their bars sit
#' on top of it rather than being crossed by it.
#'
#' The line is computed by the same summary as the points it joins -- the same
#' helper, the same confidence level -- so that it passes through them by
#' construction rather than by agreement. `geom_line()` reads only `x` and
#' `y`, so the interval the helper returns beside the point is ignored here.
#'
#' It is also given a `group` of its own. ggplot2 groups by the discrete
#' variables in the figure, which along a categorical x axis is one group per
#' point: a line drawn that way would join nothing. Grouped by colour it draws
#' one line per colour, which is the comparison the colours were asking for,
#' and ungrouped it draws the single line through every point.
#' @keywords internal
#' @noRd
gs_code_dot_geom <- function(spec) {
  # The estimates are already in the data the figure is drawn from.
  if (gs_weighted(spec)) {
    return(c(if (isTRUE(spec$dot_line)) {
               sprintf("geom_line(aes(group = %s))", gs_dot_line_group(spec))
             },
             "geom_pointrange()"))
  }
  args <- gs_dot_stat_args(spec)
  c(if (isTRUE(spec$dot_line)) {
      sprintf("stat_summary(aes(group = %s), %s, geom = \"line\")",
              gs_dot_line_group(spec), args)
    },
    sprintf("stat_summary(%s, geom = \"pointrange\")", args))
}

#' What the connecting line groups by
#' @keywords internal
#' @noRd
gs_dot_line_group <- function(spec) {
  if (nzchar(spec$group)) gs_bt(spec$group) else "1"
}

#' The variables a Kaplan-Meier fit must be computed separately within
#'
#' Anything that splits the figure into curves or panels has to split the fit
#' too: a curve fitted across two facets and then drawn in one of them would
#' be the wrong curve.
#' @keywords internal
#' @noRd
gs_km_by <- function(spec, facet_strata = FALSE) {
  by <- c(spec$group, gs_facet_col(spec, facet_strata))
  unique(by[nzchar(by)])
}

#' Helper definitions a spec's figure needs before it can be drawn
#' @keywords internal
#' @noRd
gs_code_preamble <- function(spec, facet_strata = FALSE) {
  w <- gs_weighted(spec)
  c(if (gs_uses_facet_n(spec, facet_strata)) {
      if (w) GS_FACET_N_WEIGHTED_HELPER else GS_FACET_N_HELPER
    },
    if (identical(spec$plot_type, GS_DOT)) {
      if (w) GS_SVY_SUMMARY_HELPER else gs_err_helper(spec$err_type)
    },
    if (identical(spec$plot_type, GS_KM)) {
      if (w) GS_KM_WEIGHTED_HELPER else GS_KM_HELPER
    },
    if (identical(spec$plot_type, GS_KM) && isTRUE(spec$km_risk)) {
      if (w) GS_KM_RISK_WEIGHTED_HELPER else GS_KM_RISK_HELPER
    })
}

#' Per-figure data preparation, or `character(0)` when there is none
#'
#' The panel labels are written before the survival curves are fitted, so that
#' a panel counts the people in it rather than the rows of the fit, and so that
#' the fit is split by the same column the figure is panelled by.
#' @keywords internal
#' @noRd
gs_code_prep <- function(spec, data_sym = "d", facet_strata = FALSE) {
  lines <- character()
  weighted <- gs_weighted(spec)
  if (gs_uses_facet_n(spec, facet_strata)) {
    lines <- c(lines, sprintf("%s <- add_facet_n(%s, %s%s)", data_sym, data_sym,
                              gs_dq(spec$facet),
                              if (weighted) paste0(", ", gs_dq(spec$weight))
                              else ""), "")
  }
  if (identical(spec$plot_type, GS_DOT) && weighted) {
    return(c(lines, gs_code_svy_summary_call(spec, data_sym, facet_strata), ""))
  }
  if (!identical(spec$plot_type, GS_KM)) return(lines)

  by <- gs_km_by(spec, facet_strata)
  by_arg <- if (length(by)) {
    sprintf(", by = c(%s)", paste(gs_dq(by), collapse = ", "))
  } else {
    ""
  }
  fit <- if (weighted) {
    sprintf("km <- km_data_weighted(%s, des, %s, %s%s%s)", data_sym,
            gs_dq(spec$time), gs_dq(spec$event), by_arg,
            # The design-based variance is the slow part of svykm(), so it is
            # asked for only when the band that needs it is drawn.
            if (isTRUE(spec$km_ci)) ", se = TRUE" else "")
  } else {
    sprintf("km <- km_data(%s, %s, %s%s)", data_sym, gs_dq(spec$time),
            gs_dq(spec$event), by_arg)
  }
  c(lines, fit, "")
}

#' The call that estimates a weighted Dot + Error figure's points
#' @keywords internal
#' @noRd
gs_code_svy_summary_call <- function(spec, data_sym = "d", facet_strata = FALSE) {
  by <- gs_dot_by(spec, facet_strata)
  args <- c(
    data_sym, "des", gs_dq(spec$y),
    if (length(by)) sprintf("by = c(%s)", paste(gs_dq(by), collapse = ", ")),
    sprintf("bar = %s", gs_dq(spec$err_type)),
    if (spec$err_type %in% GS_ERR_CI_TYPES) {
      sprintf("conf = %s", gs_n(spec$err_level))
    },
    if (spec$err_type %in% GS_ERR_PROP_TYPES && nzchar(spec$err_event %||% "")) {
      sprintf("event = %s", gs_dq(spec$err_event))
    })
  sprintf("est <- svy_summary(%s)", paste(args, collapse = ", "))
}

#' The symbol `ggplot()` is handed, which is not the raw data for every type
#' @keywords internal
#' @noRd
gs_plot_sym <- function(spec, data_sym = "d") {
  if (identical(spec$plot_type, GS_KM)) return("km")
  if (identical(spec$plot_type, GS_DOT) && gs_weighted(spec)) return("est")
  data_sym
}

#' Preparation and plot together: one complete, evaluable figure
#'
#' What the preview, the in-app export and the R-code tab all evaluate, which
#' is what keeps the figure on screen and the code beside it the same figure.
#' The last expression in the block is the plot.
#' @keywords internal
#' @noRd
gs_code_figure <- function(spec, data_sym = "d", facet_strata = FALSE,
                           title_expr = NULL) {
  c(gs_code_preamble(spec, facet_strata),
    gs_code_prep(spec, data_sym, facet_strata),
    gs_code_plot_block(spec, data_sym, facet_strata, title_expr))
}

#' The plot itself, as one or more statements ending in the figure
#'
#' A plain figure is a single `ggplot()` expression. A survival curve with a
#' number-at-risk table is three: the curve, the table, and the stacking of
#' one on the other.
#'
#' @param sym Symbol to assign the finished figure to, or `NULL` to leave it
#'   as the value of the block.
#' @keywords internal
#' @noRd
gs_code_plot_block <- function(spec, data_sym = "d", facet_strata = FALSE,
                               title_expr = NULL, sym = NULL) {
  assign_to <- function(lines, target) {
    if (is.null(target)) lines else c(paste0(target, " <- ", lines[1]), lines[-1])
  }
  risk <- identical(spec$plot_type, GS_KM) && isTRUE(spec$km_risk)
  plot_lines <- gs_code_plot(spec, data_sym, facet_strata, title_expr,
                             shared_x = risk)
  if (!risk) return(assign_to(plot_lines, sym))

  c(gs_code_risk_times(spec),
    assign_to(plot_lines, "p"),
    "",
    gs_code_risk_table(spec, data_sym, facet_strata),
    "",
    "# guides = \"collect\" moves the legend outside both panels, which is also",
    "# what leaves them the same width: a legend inside the curve alone would",
    "# shift its x axis away from the table's.",
    assign_to(
      c(paste0("patchwork::wrap_plots(p, tbl, ncol = 1, ",
               sprintf("heights = c(%s, 1),", gs_n(GS_RISK_HEIGHT_RATIO))),
        "                      guides = \"collect\")"),
      sym))
}

# How much taller the curve is than the table beneath it.
GS_RISK_HEIGHT_RATIO <- 3

#' The times the number at risk is counted at
#'
#' Read off the curve rather than chosen by the user, and then given to both
#' panels, so that a column of the table sits under the point of the curve it
#' describes. An x range typed into the sidebar is what the curve is being
#' looked at through, so the counts are taken across that range instead.
#' @keywords internal
#' @noRd
gs_code_risk_times <- function(spec) {
  from <- gs_num(spec$xlim_min)
  to <- gs_num(spec$xlim_max)
  c("# The curve and the table share these times, so that every count sits",
    "# under the point of the curve it belongs to.",
    sprintf("risk_min <- %s", if (is.na(from)) "0" else gs_n(from)),
    sprintf("risk_max <- %s",
            if (is.na(to)) "max(km$.time, na.rm = TRUE)" else gs_n(to)),
    "risk_times <- pretty(c(risk_min, risk_max), n = 5)",
    "risk_times <- risk_times[risk_times >= risk_min & risk_times <= risk_max]",
    "")
}

#' The number-at-risk table, as a plot of its own
#'
#' One row per curve, labelled by the same variable that colours the curves,
#' so the table is read against the legend without a key of its own.
#' @keywords internal
#' @noRd
gs_code_risk_table <- function(spec, data_sym = "d", facet_strata = FALSE) {
  by <- gs_km_by(spec, facet_strata)
  by_arg <- if (length(by)) {
    sprintf(", by = c(%s)", paste(gs_dq(by), collapse = ", "))
  } else {
    ""
  }
  y <- if (nzchar(spec$group)) gs_bt(spec$group) else "\"All\""
  lab_x <- if (nzchar(spec$lab_x)) spec$lab_x else "Time"
  weighted <- gs_weighted(spec)

  lines <- c(
    if (weighted) {
      sprintf("risk <- km_risk_weighted(%s, %s, %s, %s, risk_times%s)", data_sym,
              gs_dq(spec$time), gs_dq(spec$event), gs_dq(spec$weight), by_arg)
    } else {
      sprintf("risk <- km_risk(%s, %s, %s, risk_times%s)", data_sym,
              gs_dq(spec$time), gs_dq(spec$event), by_arg)
    },
    sprintf("tbl <- ggplot(risk, aes(x = .time, y = %s)) +", y),
    sprintf("  geom_text(aes(label = %s), size = 3.4) +",
            if (weighted) ".nrisk_label" else ".nrisk"),
    "  scale_x_continuous(limits = c(risk_min, risk_max), breaks = risk_times) +",
    # A discrete axis puts the first level at the bottom, which would have the
    # rows running against the legend they are read beside.
    "  scale_y_discrete(limits = rev) +")

  facet <- gs_code_facet(spec, facet_strata)
  if (length(facet)) lines <- c(lines, gs_indent(facet))

  c(lines,
    sprintf("  labs(x = %s, y = NULL, title = %s) +", gs_dq(lab_x),
            gs_dq(if (weighted) "Number at risk (weighted in parentheses)"
                  else "Number at risk")),
    paste0("  ", spec$theme, " +"),
    "  theme(panel.grid = element_blank())")
}

#' The ggplot2 expression for one figure, as lines of code
#'
#' The first line is unindented and every following line carries two spaces,
#' so callers can splice the block into a loop body by prefixing a constant
#' indent.
#'
#' @param spec A spec list, see `gs_spec()`.
#' @param data_sym Symbol holding the data inside the generated code.
#' @param facet_strata When `TRUE`, facet by the preview's `.strat_label`
#'   column instead of the user's facet variables.
#' @param title_expr Raw R code for the plot title. When `NULL` the spec's
#'   fixed title is used.
#' @param shared_x When `TRUE`, the x axis is pinned to the breaks the
#'   number-at-risk table underneath is drawn at.
#' @return A character vector of code lines.
#' @keywords internal
#' @noRd
gs_code_plot <- function(spec, data_sym = "d", facet_strata = FALSE,
                         title_expr = NULL, shared_x = FALSE) {
  type <- spec$plot_type
  grp_aes <- gs_group_aes(type)
  has_group <- nzchar(spec$group)

  # --- aes() ---
  mapping <- character()
  if (type == GS_KM) {
    # The axes come from km_data()'s output, not from the raw columns.
    mapping <- c("x = .time", "y = .surv")
  } else if (type == GS_DOT && gs_weighted(spec)) {
    # ...and a weighted point and its bar from svy_summary()'s.
    mapping <- c(sprintf("x = %s", if (nzchar(spec$x)) gs_bt(spec$x) else "\"\""),
                 "y = .y", "ymin = .ymin", "ymax = .ymax")
  } else if (type %in% GS_XONLY_TYPES) {
    mapping <- c(mapping, sprintf("x = %s", gs_bt(spec$x)))
  } else {
    # Box/violin/dot plots stay valid without an X-variable: everything is
    # drawn in a single unlabelled column.
    mapping <- c(mapping,
                 sprintf("x = %s", if (nzchar(spec$x)) gs_bt(spec$x) else "\"\""))
    mapping <- c(mapping, sprintf("y = %s", gs_y_expr(spec)))
  }
  if (gs_weighted(spec) && type %in% GS_WEIGHT_AES_TYPES) {
    mapping <- c(mapping, sprintf("weight = %s", gs_bt(spec$weight)))
  }
  if (has_group) {
    mapping <- c(mapping, sprintf("%s = %s", grp_aes, gs_bt(spec$group)))
    # The confidence band is filled by group as well as coloured by it.
    if (type == GS_KM && isTRUE(spec$km_ci)) {
      mapping <- c(mapping, sprintf("fill = %s", gs_bt(spec$group)))
    }
  }

  lines <- sprintf("ggplot(%s, aes(%s)) +", gs_plot_sym(spec, data_sym),
                   paste(mapping, collapse = ", "))

  # --- geoms ---
  lines <- c(lines, gs_indent(gs_code_geoms(spec, data_sym)))

  # --- palette ---
  scales <- gs_code_scales(spec, grp_aes, has_group)
  if (length(scales)) lines <- c(lines, gs_indent(scales))

  # --- coordinates ---
  if (isTRUE(shared_x)) {
    lines <- c(lines, gs_indent(paste(
      "scale_x_continuous(limits = c(risk_min, risk_max),",
      "breaks = risk_times) +")))
  } else {
    # Mutually exclusive already -- shared_x is a Kaplan-Meier curve and the
    # date axis is cleared for every type but the line plot -- but written as
    # one choice so that two x scales can never both be emitted.
    time_scale <- gs_code_time_scale(spec)
    if (length(time_scale)) lines <- c(lines, gs_indent(time_scale))
  }
  coord <- gs_code_coord(spec, shared_x)
  if (length(coord)) lines <- c(lines, gs_indent(coord))

  # --- facets ---
  facet <- gs_code_facet(spec, facet_strata)
  if (length(facet)) lines <- c(lines, gs_indent(facet))

  # --- labels ---
  labs <- gs_code_labs(spec, grp_aes, has_group, title_expr, type)
  if (length(labs)) lines <- c(lines, gs_indent(labs))

  # --- theme (always last, always present) ---
  angle <- gs_code_tick_angle(spec)
  # The blanking below has to come after the angle, not before: both set
  # axis.text.x, and the last one wins.
  trailing <- length(angle) > 0L || isTRUE(shared_x)
  lines <- c(lines, gs_indent(paste0(spec$theme, if (trailing) " +" else "")))
  if (length(angle)) {
    lines <- c(lines, gs_indent(paste0(angle, if (isTRUE(shared_x)) " +" else "")))
  }
  if (isTRUE(shared_x)) {
    # The table underneath carries the axis for both, so the curve drops it
    # rather than printing the same numbers twice, one row apart.
    lines <- c(lines, gs_indent(c(
      "theme(axis.title.x = element_blank(),",
      "      axis.text.x = element_blank(),",
      "      axis.ticks.x = element_blank())")))
  }
  lines
}

#' What the Y aesthetic reads
#'
#' The column itself, except for a proportion, which is the count of one of two
#' values rather than an average of them. That comparison is written into the
#' figure rather than done to the data beforehand, so an outcome kept as a
#' factor or as `TRUE`/`FALSE` -- which is how most people keep one -- is drawn
#' without being recoded first, and the figure says which value it counted.
#'
#' The value is quoted whatever the column's type. R compares an integer, a
#' logical, a factor and a character column against a string alike, so one
#' expression covers every way an outcome is written, and `NA` stays `NA` in
#' all of them.
#' @keywords internal
#' @noRd
gs_y_expr <- function(spec) {
  if (identical(spec$plot_type, GS_DOT) &&
      spec$err_type %in% GS_ERR_PROP_TYPES && nzchar(spec$err_event %||% "")) {
    return(sprintf("as.integer(%s == %s)", gs_bt(spec$y),
                   gs_dq(spec$err_event)))
  }
  gs_bt(spec$y)
}

#' Geom layers for a plot type, each on its own line ending in " +"
#' @keywords internal
#' @noRd
gs_code_geoms <- function(spec, data_sym = "d") {
  type <- spec$plot_type
  alpha <- gs_n(spec$alpha)
  bw <- spec$binwidth
  # The two types whose layers depend on more than the plot type are built by
  # their own functions; switch() cannot dispatch on a name held in a
  # constant, which is what GS_KM and GS_LINE are.
  out <- if (identical(type, GS_KM)) {
    gs_code_km_geoms(spec, alpha)
  } else if (identical(type, GS_LINE)) {
    gs_code_line_geoms(spec, alpha)
  } else {
    switch(
      type,
      "Boxplot" = sprintf("geom_boxplot(alpha = %s, outlier.shape = %s)",
                          alpha,
                          if (isTRUE(spec$jitter)) "NA" else "19"),
      "Violin" = sprintf("geom_violin(alpha = %s)", alpha),
      "Density" = sprintf("geom_density(adjust = %s, alpha = %s)",
                          gs_n(spec$bw_adjust), alpha),
      "Histogram" = paste0(
        "geom_histogram(",
        if (!is.na(bw)) sprintf("binwidth = %s, ", gs_n(bw)) else "",
        sprintf("alpha = %s, position = \"identity\")", alpha)),
      "Dotplot" = paste0(
        "geom_dotplot(binaxis = \"y\", stackdir = \"center\", ",
        if (!is.na(bw)) sprintf("binwidth = %s, ", gs_n(bw)) else "",
        sprintf("alpha = %s)", alpha)),
      "Scatter" = sprintf("geom_point(alpha = %s)", alpha),
      "Dot + Error" = gs_code_dot_geom(spec),
      stop("Unknown plot type: ", type, call. = FALSE)
    )
  }

  # Jitter is offered for the same three types as in ggplotgui.
  if (isTRUE(spec$jitter) && type %in% c("Boxplot", "Violin", "Dot + Error")) {
    # A fixed, local seed means that changing a cosmetic setting (such as
    # opacity) does not make observations appear to move. position_jitter()
    # restores the caller's random-number state after calculating offsets.
    jitter <- "position = position_jitter(width = 0.2, height = 0, seed = 1), alpha = 0.4"
    out <- c(out, if (identical(type, GS_DOT) && gs_weighted(spec)) {
      # The figure is drawn from the estimates, so the observations have to be
      # handed back in, with their own aesthetics: the estimates' ymin and
      # ymax are not columns of the data.
      obs <- c(sprintf("x = %s", if (nzchar(spec$x)) gs_bt(spec$x) else "\"\""),
               sprintf("y = %s", gs_y_expr(spec)),
               if (nzchar(spec$group)) sprintf("colour = %s", gs_bt(spec$group)))
      sprintf("geom_point(data = %s, aes(%s), inherit.aes = FALSE, %s)",
              data_sym, paste(obs, collapse = ", "), jitter)
    } else {
      sprintf("geom_point(%s)", jitter)
    })
  }
  # The smoother goes on top of whatever it is smoothing.
  out <- c(out, gs_code_smooth(spec))
  paste0(out, " +")
}

#' The layers of a line plot
#'
#' The ID is mapped inside `geom_line()` rather than in the figure's own
#' `aes()` deliberately: it is there to say which points belong to one line,
#' and a smoother that inherited it would be fitted per subject, which is not
#' what a trend line means. Without an ID the points are joined in x order,
#' within each colour group -- the shape a set of already-summarised means
#' over time has.
#' @keywords internal
#' @noRd
gs_code_line_geoms <- function(spec, alpha) {
  out <- if (nzchar(spec$id)) {
    sprintf("geom_line(aes(group = %s), alpha = %s)", gs_bt(spec$id), alpha)
  } else {
    sprintf("geom_line(alpha = %s)", alpha)
  }
  if (isTRUE(spec$line_points)) {
    # Smaller than ggplot2's default: the marks are there to say where the
    # line was measured, and at the default size a dense set of them covers
    # the line they belong to.
    out <- c(out, sprintf("geom_point(size = 1, alpha = %s)", alpha))
  }
  out
}

#' The LOWESS smoother layer, or `character(0)`
#'
#' `method = "loess"` is named rather than left to ggplot2, which otherwise
#' chooses between loess and a GAM by the number of observations and says so
#' in a message. Naming it keeps the same smoother on the same data at every
#' size. The formula is spelled out for the same reason: it is the default,
#' but leaving it out has ggplot2 announce it on every draw.
#'
#' The smoother inherits the figure's colour grouping, so a grouped figure
#' gets one smoother per group -- the comparison the grouping was asking for.
#' @keywords internal
#' @noRd
gs_code_smooth <- function(spec) {
  if (!isTRUE(spec$smooth)) return(character())
  # The weight is mapped here rather than on the figure: the points and lines
  # under the smoother are one row each whatever the row stands for.
  weight <- if (gs_weighted(spec)) {
    sprintf("aes(weight = %s), ", gs_bt(spec$weight))
  } else {
    ""
  }
  sprintf("geom_smooth(%smethod = \"loess\", formula = y ~ x, span = %s, se = %s)",
          weight, gs_n(spec$smooth_span),
          if (isTRUE(spec$smooth_se)) "TRUE" else "FALSE")
}

#' The layers of a Kaplan-Meier curve, in drawing order
#'
#' The band is drawn first so that the step line sits on top of it, and the
#' censoring marks last so that they sit on top of both. The band is a plain
#' ribbon between the survfit confidence limits, which interpolates between
#' event times rather than stepping; at the resolution of a figure the
#' difference is invisible, and it keeps the generated code readable.
#' @keywords internal
#' @noRd
gs_code_km_geoms <- function(spec, alpha) {
  out <- character()
  if (isTRUE(spec$km_ci)) {
    out <- c(out, sprintf(
      "geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = %s, colour = NA)",
      alpha))
  }
  out <- c(out, "geom_step(linewidth = 0.8)")
  if (isTRUE(spec$km_censor)) {
    out <- c(out, paste0(
      "geom_point(data = function(x) x[x$.ncens > 0, ], ",
      "shape = 3, size = 2, show.legend = FALSE)"))
  }
  out
}

#' The coordinate layer, or character(0) when both axes are left automatic
#'
#' `coord_cartesian()` rather than `xlim()` or a scale's `limits`: it zooms
#' the figure into the range asked for, where a scale limit drops the rows
#' outside it first -- which moves a boxplot's median, rescales a density and
#' recounts a histogram. A range is a question about what to look at, not
#' about which rows the description is of.
#'
#' Either end can be left blank on its own: `NA` there is ggplot2's own "as
#' far as the data goes". A Kaplan-Meier curve's 0-to-1 y axis is a default of
#' the same kind, so a range typed in replaces it.
#' @keywords internal
#' @noRd
gs_code_coord <- function(spec, shared_x = FALSE) {
  # With a number-at-risk table the two panels share one x scale, which is
  # already pinned to the range; see gs_code_risk_times().
  #
  # A date axis takes neither. The X-axis boxes are numeric, a date typed into
  # one reads as NA, and a number handed to a date scale is not ignored:
  # ggplot2 stops with "transform_date() works with objects of class <Date>
  # only" and no figure is drawn at all. Dropping the range is the difference
  # between an axis that is not zoomed and a figure that does not appear.
  numeric_x <- !nzchar(spec$x_time_class)
  xlim <- if (isTRUE(shared_x) || !numeric_x) {
    ""
  } else {
    gs_range_arg(spec$xlim_min, spec$xlim_max)
  }
  ylim <- gs_range_arg(spec$ylim_min, spec$ylim_max)
  if (!nzchar(ylim) && identical(spec$plot_type, GS_KM) && isTRUE(spec$km_ylim)) {
    ylim <- "c(0, 1)"
  }
  parts <- c(if (nzchar(xlim)) sprintf("xlim = %s", xlim),
             if (nzchar(ylim)) sprintf("ylim = %s", ylim))
  if (!length(parts)) return(character())
  sprintf("coord_cartesian(%s) +", paste(parts, collapse = ", "))
}

#' The date-axis scale, or `character(0)` when the axis is left to ggplot2
#'
#' Two separate questions, and either can be left unanswered. How far apart the
#' ticks are is `date_breaks`; what each one reads is `date_labels`, a strftime
#' format. Together they are what lets a column that really holds dates be
#' drawn as a run of years: the values plotted are unchanged, only the ticks
#' over them.
#'
#' `scale_x_datetime()` for a column carrying a time of day and
#' `scale_x_date()` for one that does not -- the wrong one of the two does not
#' relabel the axis, it refuses to draw it.
#' @keywords internal
#' @noRd
gs_code_time_scale <- function(spec) {
  if (!nzchar(spec$x_time_class)) return(character())
  unit <- spec$x_time_unit
  fmt <- spec$x_time_labels
  if (!nzchar(unit) && !nzchar(fmt)) return(character())

  parts <- character()
  if (nzchar(unit)) {
    every <- as.integer(spec$x_time_every)
    if (is.na(every) || every < 1L) every <- 1L
    base <- GS_X_TIME_BREAKS[[unit]]
    # "3 months" already carries its own count, so a request for every second
    # quarter is six months rather than "2 3 months".
    breaks <- if (grepl("^[0-9]", base)) {
      n <- as.integer(sub("^([0-9]+) .*$", "\\1", base))
      sprintf("%d %s", n * every, sub("^[0-9]+ ", "", base))
    } else {
      sprintf("%d %s%s", every, base, if (every == 1L) "" else "s")
    }
    parts <- c(parts, sprintf("date_breaks = %s", gs_dq(breaks)))
  }
  if (nzchar(fmt)) parts <- c(parts, sprintf("date_labels = %s", gs_dq(fmt)))

  fun <- if (identical(spec$x_time_class, "datetime")) "scale_x_datetime"
         else "scale_x_date"
  sprintf("%s(%s) +", fun, paste(parts, collapse = ", "))
}

#' One axis range as a `c(from, to)` literal, or `""` when both ends are blank
#' @keywords internal
#' @noRd
gs_range_arg <- function(from, to) {
  from <- gs_num(from)
  to <- gs_num(to)
  if (is.na(from) && is.na(to)) return("")
  sprintf("c(%s, %s)", gs_n(from), gs_n(to))
}

#' The scale layers, or character(0) when the default palette is in use
#'
#' A palette recolours the groups, so it has nothing to act on until a
#' grouping variable is set; that is why nothing is emitted without one.
#'
#' A continuous grouping variable takes `scale_*_distiller()` rather than
#' `scale_*_brewer()`: the brewer scales are discrete and would abort with
#' "Continuous value supplied to discrete scale" instead of drawing anything.
#'
#' A Kaplan-Meier curve with a confidence band uses two aesthetics for one
#' variable, so the palette has to be applied to both.
#' @keywords internal
#' @noRd
gs_code_scales <- function(spec, grp_aes, has_group) {
  if (!has_group || !nzchar(spec$palette)) return(character())
  kind <- if (isTRUE(spec$group_continuous)) "distiller" else "brewer"
  funs <- paste0(if (grp_aes == "fill") "scale_fill_" else "scale_colour_", kind)
  if (identical(spec$plot_type, GS_KM) && isTRUE(spec$km_ci)) {
    funs <- c(funs, paste0("scale_fill_", kind))
  }
  sprintf("%s(palette = %s) +", funs, gs_dq(spec$palette))
}

#' The facet layer, or character(0) when there is none
#'
#' The column panelled by is not always the column the user chose: with the
#' sizes switched on it is the labelled copy `add_facet_n()` writes, so that a
#' strip reads `site: Site A (N = 303)` rather than `Site A`.
#'
#' A plain column is panelled through `label_both()`, which names the variable
#' on the strip the way the labelled copy already does. A level is not always
#' self-explanatory -- a categorized variable's levels are bare ranges, and a
#' panel headed `[18,53]` does not say what the range is a range of.
#' @keywords internal
#' @noRd
gs_code_facet <- function(spec, facet_strata = FALSE) {
  col <- gs_facet_col(spec, facet_strata)
  if (!nzchar(col)) return(character())
  if (isTRUE(facet_strata)) {
    return("facet_wrap(~ .strat_label, scales = \"free_x\") +")
  }
  if (identical(col, GS_FACET_COL)) {
    return(sprintf("facet_wrap(~ %s) +", gs_bt(col)))
  }
  sprintf("facet_wrap(~ %s, labeller = label_both) +", gs_bt(col))
}

#' The theme() layer that turns the tick labels, or `character(0)`
#'
#' Emitted after the theme itself, because a theme replaces the whole element
#' rather than merging into it: `theme_bw()` after this would put the labels
#' back flat.
#'
#' The justification is not a setting. A label turned counter-clockwise has to
#' be pulled back towards its tick or it hangs off the end of it, so `hjust`
#' follows from the angle rather than being another thing to choose: right
#' aligned against the axis, and centred on the tick once the text is standing
#' upright.
#' @keywords internal
#' @noRd
gs_code_tick_angle <- function(spec) {
  parts <- character()
  ax <- gs_tick_angle(spec$tick_angle_x)
  ay <- gs_tick_angle(spec$tick_angle_y)
  if (ax != 0) {
    parts <- c(parts, sprintf(
      "axis.text.x = element_text(angle = %s, hjust = 1, vjust = %s)",
      gs_n(ax), if (ax == 90) "0.5" else "1"))
  }
  if (ay != 0) {
    parts <- c(parts, sprintf(
      "axis.text.y = element_text(angle = %s, hjust = %s, vjust = 0.5)",
      gs_n(ay), if (ay == 90) "0.5" else "1"))
  }
  if (!length(parts)) return(character())
  sprintf("theme(%s)", paste(parts, collapse = ", "))
}

#' The labs() layer, or character(0) when nothing is labelled
#'
#' Axis labels normally come from the column names, which is why nothing is
#' emitted unless the user typed something. A Kaplan-Meier curve is the
#' exception: its axes are `.time` and `.surv`, names that mean nothing to a
#' reader, so it names them itself unless told otherwise.
#' @keywords internal
#' @noRd
gs_code_labs <- function(spec, grp_aes, has_group, title_expr = NULL,
                         type = spec$plot_type) {
  parts <- character()
  if (!is.null(title_expr)) {
    parts <- c(parts, sprintf("title = %s", title_expr))
  } else if (nzchar(spec$title)) {
    parts <- c(parts, sprintf("title = %s", gs_dq(spec$title)))
  }
  # A date axis ticked by year is still an axis of dates, and "admit_date" is
  # not what those ticks read. Naming the unit is what the user asked the axis
  # for; a label typed by hand still wins.
  lab_x <- if (nzchar(spec$lab_x)) {
    spec$lab_x
  } else if (nzchar(spec$x_time_unit %||% "")) {
    GS_TIME_UNIT_LABEL[[spec$x_time_unit]]
  } else if (type == GS_KM) {
    "Time"
  } else {
    ""
  }
  # Left to itself the axis would read `as.integer(died == "1")`, which is
  # what the figure does rather than what it shows.
  lab_y <- if (nzchar(spec$lab_y)) {
    spec$lab_y
  } else if (identical(type, GS_DOT) && gs_weighted(spec)) {
    # Left to itself this axis would read `.y`.
    if (spec$err_type %in% GS_ERR_PROP_TYPES && nzchar(spec$err_event %||% "")) {
      sprintf("Weighted proportion %s = %s", spec$y, spec$err_event)
    } else if (spec$err_type %in% GS_ERR_PROP_TYPES) {
      sprintf("Weighted proportion of %s", spec$y)
    } else {
      sprintf("Weighted mean of %s", spec$y)
    }
  } else if (identical(type, "Histogram") && gs_weighted(spec)) {
    # A bin's height is now a sum of weights, not a number of rows.
    "Weighted count"
  } else if (identical(type, GS_DOT) && spec$err_type %in% GS_ERR_PROP_TYPES &&
             nzchar(spec$err_event %||% "")) {
    sprintf("Proportion %s = %s", spec$y, spec$err_event)
  } else if (type == GS_KM) {
    "Survival probability"
  } else {
    ""
  }
  if (nzchar(lab_x)) parts <- c(parts, sprintf("x = %s", gs_dq(lab_x)))
  if (nzchar(lab_y)) parts <- c(parts, sprintf("y = %s", gs_dq(lab_y)))
  if (has_group && nzchar(spec$lab_legend)) {
    # Both aesthetics must be renamed together, or ggplot2 splits one legend
    # into two.
    aes_named <- if (type == GS_KM && isTRUE(spec$km_ci)) c(grp_aes, "fill")
                 else grp_aes
    parts <- c(parts, sprintf("%s = %s", aes_named, gs_dq(spec$lab_legend)))
  }
  if (!length(parts)) return(character())
  sprintf("labs(%s) +", paste(parts, collapse = ", "))
}

# --- full script -------------------------------------------------------------

#' Default file-name prefix for a plot type
#' @keywords internal
#' @noRd
gs_default_prefix <- function(plot_type) {
  p <- tolower(gsub("[^A-Za-z0-9]+", "_", plot_type))
  gsub("^_+|_+$", "", p)
}

#' The prefix a spec's exported files actually start with
#'
#' Read in one place by both the export and the Strata tab's file-name column,
#' so that the name the user is shown is the name that appears on disk.
#' @keywords internal
#' @noRd
gs_export_prefix <- function(spec) {
  if (nzchar(spec$prefix)) spec$prefix else gs_default_prefix(spec$plot_type)
}

#' The copy-pasteable R code shown on the R-code tab
#'
#' What is emitted is the `ggplot2` code for **one** figure -- the figure on
#' the Plot tab -- and nothing else: no export loop, no `ggsave()`, no output
#' folder. Writing the files is the "Export all figures" button's job, and a
#' script that also did it would bury the four lines a reader actually wants to
#' take away, edit and put in their own analysis.
#'
#' Everything the figure genuinely needs is still here, in the order it has to
#' run: the categorized columns, the exclusion of rows that cannot be placed in
#' a panel, the subset that makes this figure the stratum it is, and the
#' helpers a survival curve or a labelled panel strip defines for itself.
#'
#' @param spec A spec list, see `gs_spec()`.
#' @param stratum One row of `gs_strata_table()` -- the figure being shown --
#'   or `NULL` when the layers produce a single figure.
#' @param note An extra comment line for the header, used to say when the Plot
#'   tab is showing every figure at once and this is the code for one of them.
#' @return A single string containing the code.
#' @keywords internal
#' @noRd
gs_code_script <- function(spec, stratum = NULL, note = NULL) {
  strat <- spec$strat_vars[nzchar(spec$strat_vars)]
  lv <- gs_stratum_levels(stratum, strat, spec$strat_mode)
  title <- if (!is.null(stratum) && nrow(stratum)) {
    gs_title_literal(spec, stratum$file[1L], stratum$n[1L], stratum$n_w[1L])
  } else {
    NULL
  }

  # data.table earns its library() call only when something below is written
  # in it: a categorization, the row exclusion, or the subset for this figure.
  # A plain figure needs none of the three, and is better off as ggplot2 code
  # a reader can lift without taking a dependency with it.
  needs_dt <- length(gs_as_cuts(spec$cuts)) > 0L ||
    length(gs_exclude_vars(spec)) > 0L

  header <- c(
    "# ---------------------------------------------------------------------",
    "# Generated by ggstratify",
    "# The ggplot2 code for the figure shown on the Plot tab.",
    if (!is.null(stratum) && nrow(stratum)) {
      paste0("# Figure: ", gs_label_n(stratum$label[1L], stratum$n[1L],
                                      stratum$n_w[1L]))
    },
    if (!is.null(note)) paste0("# ", note),
    "# ---------------------------------------------------------------------",
    if (needs_dt) "library(data.table)",
    "library(ggplot2)",
    ""
  )

  if (!needs_dt) {
    # Nothing to derive, exclude or subset: the figure is drawn from the data
    # as it stands.
    return(paste(c(header,
                   sprintf("d <- %s   # <- your data", spec$data_name),
                   "",
                   gs_code_figure_body(spec, title)),
                 collapse = "\n"))
  }

  header <- c(
    header,
    sprintf("dt <- as.data.table(%s)   # <- your data", spec$data_name),
    "",
    gs_code_cuts(spec$cuts, "dt"),
    gs_code_design(spec, "dt"),
    gs_code_layer_na(spec, "dt")
  )

  subset <- if (length(lv)) {
    c(sprintf("# The rows this figure is drawn from: %s.",
              paste(sprintf("%s = %s", names(lv), lv),
                    collapse = ", ")),
      sprintf("d <- dt[%s]",
              paste(sprintf("as.character(%s) == %s", gs_bt(names(lv)),
                            gs_dq(lv)),
                    collapse = " & ")),
      "")
  } else {
    c("d <- dt", "")
  }

  paste(c(header, subset, gs_code_figure_body(spec, title)), collapse = "\n")
}

#' The helpers, the preparation and the plot, ending in the figure
#'
#' The part of the generated code that is the same however the data reached
#' `d`: with a subset in front of it, or straight from the user's object.
#' @keywords internal
#' @noRd
gs_code_figure_body <- function(spec, title_expr = NULL) {
  c(gs_code_preamble(spec),
    gs_code_prep(spec, data_sym = "d"),
    gs_code_plot_block(spec, data_sym = "d", title_expr = title_expr,
                       sym = "p"),
    "",
    "p")
}

#' The level of each stratifying variable that one figure stands for
#'
#' `gs_strata_table()` reports a crossed stratum as one row whose `level`
#' joins the levels with `" | "`, which is what the Strata tab shows; the
#' generated subset needs them apart again.
#'
#' @return A named character vector, `variable = level`, or `character(0)`
#'   when the row cannot be taken apart with confidence -- a level containing
#'   the separator itself, say -- in which case no subset is generated rather
#'   than a wrong one.
#' @keywords internal
#' @noRd
gs_stratum_levels <- function(row, strat_vars, mode = "independent") {
  if (is.null(row) || !nrow(row) || !length(strat_vars)) return(character())
  if (!identical(mode, "crossed")) {
    v <- as.character(row$var[1L])
    if (!v %in% strat_vars) return(character())
    return(stats::setNames(as.character(row$level[1L]), v))
  }
  parts <- strsplit(as.character(row$level[1L]), " | ", fixed = TRUE)[[1L]]
  if (length(parts) != length(strat_vars)) return(character())
  stats::setNames(parts, strat_vars)
}

#' The title of one already-drawn figure, as a quoted string literal
#'
#' The preview and the in-app export know the stratum's label and size
#' outright, so they build the title here rather than as an expression. The
#' rule is `gs_title_expr()`'s rule, applied to values instead of code.
#'
#' @param spec A spec list.
#' @param label The stratum's name, used when the user typed no title.
#' @param n The stratum size, or `NULL` to leave the size off.
#' @param n_w The stratum's sum of weights, or `NULL` for an unweighted figure.
#' @return A quoted string literal, or `NULL` when there is no title.
#' @keywords internal
#' @noRd
gs_title_literal <- function(spec, label = "", n = NULL, n_w = NULL) {
  base <- if (nzchar(spec$title)) spec$title else label
  if (!nzchar(base)) return(NULL)
  gs_dq(if (isTRUE(spec$show_n) && !is.null(n)) gs_label_n(base, n, n_w) else base)
}

#' Evaluate generated plot code against a data.table
#'
#' Used by both the live preview and the in-app export so that neither can
#' diverge from the code on the R-code tab. The block may contain more than
#' one expression -- a Kaplan-Meier figure defines its helper and fits the
#' curves first -- in which case the value of the last one, the plot, is
#' returned.
#'
#' @param code_lines Output of `gs_code_figure()`.
#' @param data The data to bind to the code's data symbol.
#' @param data_sym The symbol name used when the code was generated.
#' @param design The survey design a design-based figure reads as `des`, from
#'   `gs_build_design()`; `NULL` for every other figure.
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
gs_eval_plot <- function(code_lines, data, data_sym = "d", design = NULL) {
  # The package namespace, not ggplot2's: the generated code is written for a
  # session that has attached both ggplot2 and data.table, and data.table
  # quietly falls back to data.frame semantics when it is called from a
  # namespace that does not import it.
  env <- new.env(parent = asNamespace("ggstratify"))
  assign(data_sym, data, envir = env)
  if (!is.null(design)) assign("des", design, envir = env)
  eval(parse(text = paste(code_lines, collapse = "\n")), envir = env)
}
