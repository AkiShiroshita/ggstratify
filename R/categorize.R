# Turning a continuous variable into a categorical one. A "cut" is a small
# plain list describing one such rule; the spec carries a list of them.
#
# There is one implementation, not two: `gs_code_cuts()` writes the
# `data.table` lines, and `gs_apply_cuts()` runs those very lines against the
# data the app is holding. The derived column in the app is therefore always
# the column the generated script produces.

#' Build one categorization rule with defaults filled in
#'
#' @param var Name of the continuous column to categorize.
#' @param new Name of the derived column. Defaults to `<var>_cat`.
#' @param method One of `"quantile"` (equal-sized groups), `"equal"`
#'   (equal-width bins) or `"breaks"` (user-supplied cut points).
#' @param n Number of groups, for the first two methods.
#' @param breaks Internal cut points, for `method = "breaks"`. They are
#'   interpreted as boundaries between open-ended bins, so `65` gives
#'   `(-Inf, 65]` and `(65, Inf]`.
#' @return A list with elements `var`, `new`, `method`, `n`, `breaks`.
#' @keywords internal
#' @noRd
gs_cut <- function(var, new = NULL, method = "quantile", n = 4L,
                   breaks = numeric()) {
  method <- match.arg(method, unname(GS_CUT_METHODS))
  n <- as.integer(gs_num(n, 4))
  if (is.na(n)) n <- 4L
  breaks <- suppressWarnings(as.numeric(breaks))
  breaks <- sort(unique(breaks[is.finite(breaks)]))
  var <- as.character(var)
  if (is.null(new) || !nzchar(new)) new <- paste0(var, "_cat")
  list(var = var, new = as.character(new), method = method,
       n = max(n, 2L), breaks = breaks)
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
                                                "breaks"))])
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
gs_cut_name <- function(var, taken = character()) {
  base <- paste0(var, "_cat")
  if (!base %in% taken) return(base)
  for (i in 2:99) {
    cand <- paste0(base, i)
    if (!cand %in% taken) return(cand)
  }
  base
}

#' A one-line human description of a cut, for the sidebar list
#' @keywords internal
#' @noRd
gs_cut_describe <- function(cut) {
  how <- switch(
    cut$method,
    quantile = sprintf("%d quantile groups", cut$n),
    equal    = sprintf("%d equal-width bins", cut$n),
    breaks   = paste0("cut at ", paste(gs_n_vec(cut$breaks), collapse = ", "))
  )
  sprintf("%s <- %s (%s)", cut$new, cut$var, how)
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
    res <- try(eval(parse(text = line), envir = env), silent = TRUE)
    if (inherits(res, "try-error")) {
      errors <- c(errors, sprintf("%s: %s", cut$new,
                                  conditionMessage(attr(res, "condition"))))
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
  problems <- character()
  if (!is.numeric(dt[[cut$var]])) {
    problems <- c(problems, sprintf(
      "'%s' is not numeric; only continuous variables can be categorized.",
      cut$var))
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
  used <- sum(table(trial[[cut$new]]) > 0L)
  if (used < 2L) {
    return(sprintf(
      "This gives fewer than two non-empty groups of '%s'; try fewer groups or other cut points.",
      cut$var))
  }
  character()
}
