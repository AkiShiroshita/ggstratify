# Turning a continuous variable into a categorical one. A "cut" is a small
# plain list describing one such rule; the spec carries a list of them.
#
# There is one implementation, not two: `gs_code_cuts()` writes the
# `data.table` lines, and `gs_apply_cuts()` runs those very lines against the
# data the app is holding. The derived column in the app is therefore always
# the column the generated script produces.

#' Build one categorization rule with defaults filled in
#'
#' @param var Name of the column to categorize. Continuous for every method
#'   but `"missing"`, which reads no values and so takes a column of any type.
#' @param new Name of the derived column. Defaults to `<var>_cat`, or to
#'   `<var>_missing` for `method = "missing"`.
#' @param method One of `"quantile"` (equal-sized groups), `"equal"`
#'   (equal-width bins), `"breaks"` (user-supplied cut points) or `"missing"`
#'   (two groups: the rows where the column has a value and the rows where it
#'   does not).
#' @param n Number of groups, for the first two methods.
#' @param breaks Internal cut points, for `method = "breaks"`. They are
#'   interpreted as boundaries between open-ended bins, so `65` gives
#'   `(-Inf, 65]` and `(65, Inf]`.
#' @param unit The resolution, for `method = "period"`. One of
#'   `GS_TIME_UNIT_VALUES`: a calendar period, which floors the column to the
#'   start of that period and leaves it a date, or a position in the cycle,
#'   which pools every year together and gives a factor.
#' @param season_start The month spring starts in, for `unit = "season"`. The
#'   four seasons follow it three months at a time, so `3` is the northern
#'   hemisphere and `9` the southern.
#' @return A list with elements `var`, `new`, `method`, `n`, `breaks`, `unit`
#'   and `season_start`.
#' @keywords internal
#' @noRd
gs_cut <- function(var, new = NULL, method = "quantile", n = 4L,
                   breaks = numeric(), unit = "month",
                   season_start = GS_SEASON_START) {
  method <- match.arg(method, unname(GS_CUT_METHODS))
  n <- as.integer(gs_num(n, 4))
  if (is.na(n)) n <- 4L
  breaks <- suppressWarnings(as.numeric(breaks))
  breaks <- sort(unique(breaks[is.finite(breaks)]))
  unit <- as.character(unit %||% "month")
  unit <- if (length(unit) && unit[[1L]] %in% GS_TIME_UNIT_VALUES) unit[[1L]] else "month"
  # A month number, so anything outside 1:12 is not a season anyone meant.
  season_start <- as.integer(gs_num(season_start, GS_SEASON_START))
  if (is.na(season_start) || season_start < 1L || season_start > 12L) {
    season_start <- GS_SEASON_START
  }
  var <- as.character(var)
  if (is.null(new) || !nzchar(new)) {
    new <- paste0(var, gs_cut_suffix(method, unit))
  }
  # n is floored at 2: one group is not a categorization, and every caller
  # that builds a cut goes through here, so nothing downstream has to carry
  # the case. gs_check_cut() still holds the lower bound as well, for a cut
  # assembled as a plain list without this constructor.
  list(var = var, new = as.character(new), method = method,
       n = max(n, 2L), breaks = breaks, unit = unit,
       season_start = season_start)
}

#' The suffix a derived column's default name ends in
#'
#' Keyed by method for every method but `"period"`, whose suffix names the
#' resolution instead: a date read by year and the same date read by month are
#' two different columns, and calling both `admit_period` would say nothing
#' about which is which.
#' @keywords internal
#' @noRd
gs_cut_suffix <- function(method, unit = "month") {
  if (identical(method, "period")) {
    if (!unit %in% GS_TIME_UNIT_VALUES) unit <- "month"
    return(unname(GS_TIME_SUFFIX[[unit]]))
  }
  unname(GS_CUT_SUFFIX[[match.arg(method, unname(GS_CUT_METHODS))]])
}

#' Coerce whatever was handed to `gs_spec()` into a list of cuts
#' @keywords internal
#' @noRd
gs_as_cuts <- function(cuts) {
  if (is.null(cuts) || !length(cuts)) return(list())
  # A single cut, passed unwrapped.
  if (!is.null(cuts$var)) cuts <- list(cuts)
  lapply(cuts, function(cut) {
    do.call(gs_cut, cut[intersect(names(cut), c("var", "new", "method", "n",
                                                "breaks", "unit",
                                                "season_start"))])
  })
}

#' Read cut points typed into a text box
#'
#' Commas, semicolons and whitespace all separate. `NA` in the result means
#' the user typed something that is not a number, which the caller reports.
#' @keywords internal
#' @noRd
gs_parse_breaks <- function(x) {
  parts <- unlist(strsplit(as.character(x %||% ""), "[,;[:space:]]+"))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(numeric())
  suppressWarnings(as.numeric(parts))
}

#' A default name for a derived column that does not clash with the data
#' @keywords internal
#' @noRd
gs_cut_name <- function(var, taken = character(), method = "quantile",
                        unit = "month") {
  base <- paste0(var, gs_cut_suffix(method, unit))
  if (!base %in% taken) return(base)
  # Unbounded: a capped search that gives up and returns `base` would hand
  # back the very name it was asked to avoid. The loop ends on the first free
  # candidate, so it runs `length(taken)` times at the very worst.
  i <- 2L
  repeat {
    cand <- paste0(base, i)
    if (!cand %in% taken) return(cand)
    i <- i + 1L
  }
}

#' A one-line human description of a cut, for the sidebar list
#' @keywords internal
#' @noRd
gs_cut_describe <- function(cut) {
  how <- switch(
    cut$method,
    quantile = sprintf("%d quantile groups", cut$n),
    equal    = sprintf("%d equal-width bins", cut$n),
    breaks   = paste0("cut at ", paste(gs_n_vec(cut$breaks), collapse = ", ")),
    missing  = "missing vs observed",
    period   = gs_time_describe(cut)
  )
  sprintf("%s <- %s (%s)", cut$new, cut$var, how)
}

#' How a time resolution reads in the sidebar list
#'
#' A season names the month it starts in, because two rules differing only in
#' where spring begins would otherwise be indistinguishable in that list.
#' @keywords internal
#' @noRd
gs_time_describe <- function(cut) {
  switch(
    cut$unit,
    month_of_year   = "by month of the year",
    season          = sprintf("by season, spring starting in %s",
                              month.name[[cut$season_start]]),
    quarter_of_year = "by quarter of the year",
    day_of_week     = "by day of the week",
    hour_of_day     = "by hour of the day",
    sprintf("by %s", cut$unit)
  )
}

#' Format a numeric vector for display
#' @keywords internal
#' @noRd
gs_n_vec <- function(x) vapply(x, gs_n, character(1L))

#' Drop cuts whose source column is missing, and cuts that collide
#'
#' Later cuts can legitimately read a column an earlier cut created, so the
#' set of known names grows as the list is walked.
#' @keywords internal
#' @noRd
gs_valid_cuts <- function(cuts, nms) {
  out <- list()
  for (cut in gs_as_cuts(cuts)) {
    if (!cut$var %in% nms) next
    if (cut$new %in% nms) next
    out[[length(out) + 1L]] <- cut
    nms <- c(nms, cut$new)
  }
  out
}

#' Add the derived columns to a copy of the data
#'
#' The copy is what keeps the caller's table untouched; the derived columns
#' are then written into it by reference.
#'
#' A rule that fails (non-unique quantile breaks, say) is skipped rather than
#' aborting the app, and its message is attached as the `gs_cut_error`
#' attribute so the caller can report it.
#'
#' @param dt A `data.table`.
#' @param cuts A list of cuts.
#' @return A `data.table` with one extra factor column per applied cut.
#' @keywords internal
#' @noRd
gs_apply_cuts <- function(dt, cuts) {
  cuts <- gs_valid_cuts(cuts, names(dt))
  if (!length(cuts)) return(dt)

  out <- data.table::copy(dt)
  # data.table's namespace, so that `:=` and friends resolve exactly as they
  # do in the generated script, which starts with library(data.table).
  env <- new.env(parent = asNamespace("data.table"))
  assign("dt", out, envir = env)

  errors <- character()
  for (cut in cuts) {
    line <- gs_code_cut_line(cut, "dt")
    # tryCatch rather than try(): the message comes from the condition the
    # handler is given, instead of from an attribute of the returned object.
    msg <- NULL
    tryCatch(eval(parse(text = line), envir = env),
             error = function(e) msg <<- conditionMessage(e))
    if (!is.null(msg)) {
      errors <- c(errors, sprintf("%s: %s", cut$new, msg))
      if (cut$new %in% names(out)) out[, (cut$new) := NULL]
    }
  }
  if (length(errors)) attr(out, "gs_cut_error") <- errors
  out[]
}

#' Check that one cut can be applied to the data
#'
#' Run before a cut is added, so that a mistake is reported as a notification
#' instead of appearing as a broken column.
#'
#' @param dt A `data.table`.
#' @param cut A cut, see `gs_cut()`.
#' @return A character vector of problems; empty when the cut is usable.
#' @keywords internal
#' @noRd
gs_check_cut <- function(dt, cut) {
  if (!cut$var %in% names(dt)) {
    return(sprintf("'%s' is not a column in the data.", cut$var))
  }
  col <- dt[[cut$var]]
  problems <- character()
  # "missing" reads no values, so it has nothing to require of the type. Every
  # other method cuts the values themselves and needs numbers to cut.
  if (cut$method %in% GS_VALUE_CUT_METHODS && !is.numeric(col)) {
    problems <- c(problems, sprintf(
      "'%s' is not numeric; only numeric variables can be categorized.",
      cut$var))
  }
  # A time resolution asks a question only a moment in time can answer, and
  # each of these is asked here rather than left to the trial run below. The
  # trial run does catch them, but it reports whatever data.table said -- "do
  # not know how to convert 'x' to class Date" -- or, worse, succeeds and
  # leaves one group, which the generic message at the end of this function
  # then blames on having too many groups.
  if (identical(cut$method, "period")) {
    problems <- c(problems, gs_check_period(col, cut))
  }
  if (!nzchar(cut$new)) {
    problems <- c(problems, "Give the new variable a name.")
  } else if (cut$new %in% names(dt)) {
    problems <- c(problems, sprintf(
      "'%s' already exists in the data; choose another name.", cut$new))
  }
  if (cut$method %in% c("quantile", "equal") && (cut$n < 2L || cut$n > 20L)) {
    problems <- c(problems, "The number of groups must be between 2 and 20.")
  }
  if (cut$method == "breaks" && !length(cut$breaks)) {
    problems <- c(problems, "Give at least one cut point.")
  }
  if (length(problems)) return(problems)

  # Only the source column is copied, so the trial run stays cheap on wide
  # data.
  trial <- gs_apply_cuts(dt[, cut$var, with = FALSE], list(cut))
  err <- attr(trial, "gs_cut_error")
  if (length(err)) return(err)
  if (!cut$new %in% names(trial)) {
    return(sprintf("'%s' could not be categorized this way.", cut$var))
  }
  # A day resolution over a few years of data has as many groups as there are
  # days, and table() would build a vector that long only to count how many of
  # its entries are non-zero. uniqueN() answers the same question directly.
  # The other methods still go through table(), because the "missing" branch
  # below reads one of its entries by name.
  if (identical(cut$method, "period")) {
    if (data.table::uniqueN(trial[[cut$new]], na.rm = TRUE) >= 2L) {
      return(character())
    }
    return(sprintf(
      "Reading '%s' by %s gives one group, not two.", cut$var, cut$unit))
  }
  counts <- table(trial[[cut$new]])
  used <- sum(counts > 0L)
  if (used < 2L) {
    # One group is not a stratification. Which one it is says what went wrong,
    # and for "missing" it is the answer to the question rather than an error:
    # the variable is complete.
    if (identical(cut$method, "missing")) {
      return(if (counts[[GS_MISSING_LABEL]] == 0L) {
        sprintf("'%s' has no missing values, so this would make one group, not two.",
                cut$var)
      } else {
        sprintf("'%s' is missing in every row, so this would make one group, not two.",
                cut$var)
      })
    }
    return(sprintf(
      "This gives fewer than two non-empty groups of '%s'; try fewer groups or other cut points.",
      cut$var))
  }
  character()
}

#' What a time resolution needs of the column it reads
#'
#' Three separate requirements, kept apart because the answer to "why not?" is
#' different each time and the user is owed the one that applies.
#' @keywords internal
#' @noRd
gs_check_period <- function(col, cut) {
  if (!gs_is_temporal_col(col)) {
    return(sprintf(paste0("'%s' is not a date or a time; only a date or ",
                          "date-time variable has a time resolution to read."),
                   cut$var))
  }
  if (!length(col[!is.na(col)])) {
    return(sprintf("'%s' has no values to read a time resolution from.",
                   cut$var))
  }
  # An hour taken from a plain date is 0 for every row -- data.table answers
  # rather than refusing -- so this would quietly make a column with one value.
  if (cut$unit %in% GS_TIME_OF_DAY_UNITS && !gs_has_clock(col)) {
    return(sprintf(paste0("'%s' is a date with no time of day, so every row ",
                          "would fall in the same %s."),
                   cut$var, if (identical(cut$unit, "minute")) "minute" else "hour"))
  }
  # The mirror image: a time of day has no calendar to place it in, and
  # data.table errors rather than answering.
  if (!cut$unit %in% GS_TIME_OF_DAY_UNITS && !gs_has_calendar(col)) {
    return(sprintf(paste0("'%s' is a time of day with no date, so there is no ",
                          "calendar period to place it in."), cut$var))
  }
  character()
}
