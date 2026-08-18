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

#' The `cut()` expression for one categorization rule
#' @keywords internal
#' @noRd
gs_cut_expr <- function(cut) {
  v <- gs_bt(cut$var)
  switch(
    cut$method,
    quantile = sprintf(paste0("cut(%s, breaks = unique(stats::quantile(%s, ",
                              "probs = seq(0, 1, length.out = %s), na.rm = TRUE)), ",
                              "include.lowest = TRUE)"),
                       v, v, gs_n(cut$n + 1L)),
    equal    = sprintf("cut(%s, breaks = %s)", v, gs_n(cut$n)),
    breaks   = sprintf("cut(%s, breaks = c(-Inf, %s, Inf))", v,
                       paste(gs_n_vec(cut$breaks), collapse = ", ")),
    stop("Unknown cut method: ", cut$method, call. = FALSE)
  )
}

#' The single `data.table` line that adds one derived column
#' @keywords internal
#' @noRd
gs_code_cut_line <- function(cut, data_sym = "dt") {
  sprintf("%s[, %s := %s]", data_sym, gs_bt(cut$new), gs_cut_expr(cut))
}

#' The block that adds every derived column, or `character(0)`
#'
#' Emitted once, straight after the data is loaded, so that a categorized
#' variable can be used anywhere a real column can -- including as a
#' stratifying variable.
#' @keywords internal
#' @noRd
gs_code_cuts <- function(cuts, data_sym = "dt") {
  cuts <- gs_as_cuts(cuts)
  if (!length(cuts)) return(character())
  c("# Categorized continuous variables.",
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
  vars <- gs_layer_vars(spec)
  if (!length(vars)) return(character())
  c("# Rows with no value for a layer variable cannot be placed in a panel or",
    "# a figure, so they are excluded here -- once, before anything is counted.",
    sprintf("layer_vars <- c(%s)", paste(gs_dq(vars), collapse = ", ")),
    "for (v in layer_vars) {",
    sprintf("  n_na <- sum(is.na(%s[[v]]))", data_sym),
    "  if (n_na > 0L) {",
    "    message(\"Excluded \", n_na, \" row(s) with a missing \", v, \".\")",
    "  }",
    sprintf("  %s <- %s[!is.na(%s[[v]])]", data_sym, data_sym, data_sym),
    "}",
    "")
}

# --- Kaplan-Meier ------------------------------------------------------------

# Column names km_data() and km_risk() produce. Dot-prefixed so that they
# cannot collide with a grouping variable the user is fitting by;
# gs_clean_names() keeps the data free of them for the same reason.
GS_KM_COLS <- c(".time", ".surv", ".lower", ".upper", ".ncens", ".nrisk")

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
  c(if (gs_uses_facet_n(spec, facet_strata)) GS_FACET_N_HELPER,
    if (identical(spec$plot_type, GS_KM)) GS_KM_HELPER,
    if (identical(spec$plot_type, GS_KM) && isTRUE(spec$km_risk)) {
      GS_KM_RISK_HELPER
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
  if (gs_uses_facet_n(spec, facet_strata)) {
    lines <- c(lines, sprintf("%s <- add_facet_n(%s, %s)", data_sym, data_sym,
                              gs_dq(spec$facet)), "")
  }
  if (!identical(spec$plot_type, GS_KM)) return(lines)

  by <- gs_km_by(spec, facet_strata)
  by_arg <- if (length(by)) {
    sprintf(", by = c(%s)", paste(gs_dq(by), collapse = ", "))
  } else {
    ""
  }
  c(lines,
    sprintf("km <- km_data(%s, %s, %s%s)", data_sym, gs_dq(spec$time),
            gs_dq(spec$event), by_arg),
    "")
}

#' The symbol `ggplot()` is handed, which is not the raw data for every type
#' @keywords internal
#' @noRd
gs_plot_sym <- function(spec, data_sym = "d") {
  if (identical(spec$plot_type, GS_KM)) "km" else data_sym
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

  lines <- c(
    sprintf("risk <- km_risk(%s, %s, %s, risk_times%s)", data_sym,
            gs_dq(spec$time), gs_dq(spec$event), by_arg),
    sprintf("tbl <- ggplot(risk, aes(x = .time, y = %s)) +", y),
    "  geom_text(aes(label = .nrisk), size = 3.4) +",
    "  scale_x_continuous(limits = c(risk_min, risk_max), breaks = risk_times) +",
    # A discrete axis puts the first level at the bottom, which would have the
    # rows running against the legend they are read beside.
    "  scale_y_discrete(limits = rev) +")

  facet <- gs_code_facet(spec, facet_strata)
  if (length(facet)) lines <- c(lines, gs_indent(facet))

  c(lines,
    sprintf("  labs(x = %s, y = NULL, title = \"Number at risk\") +",
            gs_dq(lab_x)),
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
  } else if (type %in% GS_XONLY_TYPES) {
    mapping <- c(mapping, sprintf("x = %s", gs_bt(spec$x)))
  } else {
    # Box/violin/dot plots stay valid without an X-variable: everything is
    # drawn in a single unlabelled column.
    mapping <- c(mapping,
                 sprintf("x = %s", if (nzchar(spec$x)) gs_bt(spec$x) else "\"\""))
    mapping <- c(mapping, sprintf("y = %s", gs_bt(spec$y)))
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
  lines <- c(lines, gs_indent(gs_code_geoms(spec)))

  # --- palette ---
  scales <- gs_code_scales(spec, grp_aes, has_group)
  if (length(scales)) lines <- c(lines, gs_indent(scales))

  # --- coordinates ---
  if (isTRUE(shared_x)) {
    lines <- c(lines, gs_indent(paste(
      "scale_x_continuous(limits = c(risk_min, risk_max),",
      "breaks = risk_times) +")))
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
  if (isTRUE(shared_x)) {
    # The table underneath carries the axis for both, so the curve drops it
    # rather than printing the same numbers twice, one row apart.
    lines <- c(lines, gs_indent(paste0(spec$theme, " +")),
               gs_indent(c(
                 "theme(axis.title.x = element_blank(),",
                 "      axis.text.x = element_blank(),",
                 "      axis.ticks.x = element_blank())")))
  } else {
    lines <- c(lines, gs_indent(spec$theme))
  }
  lines
}

#' Geom layers for a plot type, each on its own line ending in " +"
#' @keywords internal
#' @noRd
gs_code_geoms <- function(spec) {
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
      "Dot + Error" = "stat_summary(fun.data = mean_se, geom = \"pointrange\")",
      stop("Unknown plot type: ", type, call. = FALSE)
    )
  }

  # Jitter is offered for the same three types as in ggplotgui.
  if (isTRUE(spec$jitter) && type %in% c("Boxplot", "Violin", "Dot + Error")) {
    # A fixed, local seed means that changing a cosmetic setting (such as
    # opacity) does not make observations appear to move. position_jitter()
    # restores the caller's random-number state after calculating offsets.
    out <- c(out, paste0(
      "geom_point(position = position_jitter(width = 0.2, height = 0, ",
      "seed = 1), alpha = 0.4)"))
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
    out <- c(out, sprintf("geom_point(alpha = %s)", alpha))
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
  sprintf("geom_smooth(method = \"loess\", formula = y ~ x, span = %s, se = %s)",
          gs_n(spec$smooth_span),
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
  xlim <- if (isTRUE(shared_x)) "" else gs_range_arg(spec$xlim_min, spec$xlim_max)
  ylim <- gs_range_arg(spec$ylim_min, spec$ylim_max)
  if (!nzchar(ylim) && identical(spec$plot_type, GS_KM) && isTRUE(spec$km_ylim)) {
    ylim <- "c(0, 1)"
  }
  parts <- c(if (nzchar(xlim)) sprintf("xlim = %s", xlim),
             if (nzchar(ylim)) sprintf("ylim = %s", ylim))
  if (!length(parts)) return(character())
  sprintf("coord_cartesian(%s) +", paste(parts, collapse = ", "))
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
  lab_x <- if (nzchar(spec$lab_x)) spec$lab_x else if (type == GS_KM) "Time" else ""
  lab_y <- if (nzchar(spec$lab_y)) spec$lab_y
           else if (type == GS_KM) "Survival probability" else ""
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
    gs_title_literal(spec, stratum$file[1L], stratum$n[1L])
  } else {
    NULL
  }

  # data.table earns its library() call only when something below is written
  # in it: a categorization, the row exclusion, or the subset for this figure.
  # A plain figure needs none of the three, and is better off as ggplot2 code
  # a reader can lift without taking a dependency with it.
  needs_dt <- length(gs_as_cuts(spec$cuts)) > 0L ||
    length(gs_layer_vars(spec)) > 0L

  header <- c(
    "# ---------------------------------------------------------------------",
    "# Generated by ggstratify",
    "# The ggplot2 code for the figure shown on the Plot tab.",
    if (!is.null(stratum) && nrow(stratum)) {
      sprintf("# Figure: %s (N = %d)", stratum$label[1L], stratum$n[1L])
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
#' @return A quoted string literal, or `NULL` when there is no title.
#' @keywords internal
#' @noRd
gs_title_literal <- function(spec, label = "", n = NULL) {
  base <- if (nzchar(spec$title)) spec$title else label
  if (!nzchar(base)) return(NULL)
  gs_dq(if (isTRUE(spec$show_n) && !is.null(n)) gs_label_n(base, n) else base)
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
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
gs_eval_plot <- function(code_lines, data, data_sym = "d") {
  # The package namespace, not ggplot2's: the generated code is written for a
  # session that has attached both ggplot2 and data.table, and data.table
  # quietly falls back to data.frame semantics when it is called from a
  # namespace that does not import it.
  env <- new.env(parent = asNamespace("ggstratify"))
  assign(data_sym, data, envir = env)
  eval(parse(text = paste(code_lines, collapse = "\n")), envir = env)
}
