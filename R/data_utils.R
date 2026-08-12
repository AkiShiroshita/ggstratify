# Data handling helpers. Everything here is data.table so that the app stays
# responsive on large data sets; the results that are expensive to compute
# (variable classification, stratum counts) are computed once and cached by the
# server rather than recomputed on every input change.

#' Coerce an input into a standalone data.table
#'
#' Accepts a data.frame / data.table / matrix, or a path to a delimited or
#' `.rds` file. The result is always a fresh `data.table` that shares no
#' columns with the caller's object, so the app can add helper columns by
#' reference without mutating the user's data.
#'
#' @param x A data.frame-like object or a file path.
#' @return A `data.table`.
#' @keywords internal
#' @noRd
gs_prepare_data <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    return(gs_read_file(x))
  }
  if (is.matrix(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x)) {
    stop("`dataset` must be a data.frame, a matrix or a path to a data file.",
         call. = FALSE)
  }
  # copy() first: as.data.table()/setDT() on a data.table would otherwise let
  # later `:=` calls write straight into the caller's object.
  dt <- data.table::as.data.table(data.table::copy(x))
  gs_clean_names(dt)
}

#' Read a data file into a data.table
#' @keywords internal
#' @noRd
gs_read_file <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  dt <- switch(
    ext,
    rds = {
      obj <- readRDS(path)
      if (!is.data.frame(obj)) {
        stop("The .rds file does not contain a data.frame.", call. = FALSE)
      }
      data.table::as.data.table(data.table::copy(obj))
    },
    # fread auto-detects the separator for csv/tsv/txt and anything else.
    data.table::fread(path, stringsAsFactors = FALSE, data.table = TRUE)
  )
  gs_clean_names(dt)
}

#' Drop empty/duplicated column names so that generated code is always valid
#' @keywords internal
#' @noRd
gs_clean_names <- function(dt) {
  nms <- names(dt)
  blank <- is.na(nms) | !nzchar(nms)
  if (any(blank)) nms[blank] <- paste0("V", seq_along(nms))[blank]
  if (anyDuplicated(nms)) nms <- make.unique(nms, sep = "_")
  # Reserved names used by the preview machinery and by the Kaplan-Meier
  # fit, which would otherwise be overwritten by a column of the same name.
  reserved <- c(".strat_var", ".strat_level", ".strat_label", ".gs_stratum",
                GS_FACET_COL, GS_KM_COLS)
  nms[nms %in% reserved] <- paste0(nms[nms %in% reserved], "_")
  data.table::setnames(dt, nms)
  dt[]
}

#' Classify every column as continuous / categorical
#'
#' Computed once per data set. `n_levels` is what the UI shows next to each
#' stratifying variable, and what decides whether a variable is a sensible
#' thing to stratify on at all.
#'
#' @param dt A `data.table`.
#' @param max_levels Largest number of distinct values still treated as
#'   categorical for stratification purposes.
#' @param min_continuous Smallest number of distinct numeric values before a
#'   numeric column counts as continuous rather than a coded category.
#' @return A `data.table` with one row per column.
#' @keywords internal
#' @noRd
gs_classify_vars <- function(dt, max_levels = 50L, min_continuous = 10L) {
  nms <- names(dt)
  if (!length(nms)) {
    return(data.table::data.table(
      var = character(), class = character(), n_levels = integer(),
      n_missing = integer(), is_continuous = logical(),
      is_categorical = logical(), is_numeric = logical(), is_event = logical(),
      can_stratify = logical()
    ))
  }

  info <- data.table::rbindlist(lapply(nms, function(v) {
    col <- dt[[v]]
    cls <- class(col)[1L]
    nlev <- data.table::uniqueN(col, na.rm = TRUE)
    numericish <- is.numeric(col) && !inherits(col, c("Date", "POSIXct"))
    list(
      var = v,
      class = cls,
      n_levels = as.integer(nlev),
      n_missing = as.integer(sum(is.na(col))),
      is_continuous = numericish && nlev >= min_continuous,
      is_categorical = !numericish || nlev < min_continuous,
      # Both are for the Kaplan-Meier selectors: survival::Surv() needs a
      # numeric time and a two-valued event indicator.
      is_numeric = numericish,
      is_event = gs_is_event_col(col)
    )
  }))

  # A variable is worth stratifying on when it has at least two levels but
  # few enough that one figure per level is still meaningful.
  info[, can_stratify := is_categorical & n_levels >= 2L & n_levels <= max_levels]
  info[]
}

#' Can a column be handed to `survival::Surv()` as the event indicator?
#'
#' Surv() accepts logical, 0/1 and 1/2. A factor is rejected even when it has
#' exactly two levels, so it is rejected here too rather than left to fail
#' inside the fit.
#' @keywords internal
#' @noRd
gs_is_event_col <- function(col) {
  if (is.logical(col)) return(TRUE)
  if (!is.numeric(col) || inherits(col, c("Date", "POSIXct"))) return(FALSE)
  vals <- unique(col[!is.na(col)])
  if (!length(vals) || length(vals) > 2L) return(FALSE)
  all(vals %in% c(0, 1)) || all(vals %in% c(1, 2))
}

#' Names of columns matching a role
#' @keywords internal
#' @noRd
gs_vars_of <- function(info, role = c("continuous", "categorical", "stratify",
                                      "numeric", "event", "all")) {
  role <- match.arg(role)
  if (is.null(info) || !nrow(info)) return(character())
  switch(
    role,
    continuous  = info[is_continuous == TRUE, var],
    categorical = info[is_categorical == TRUE, var],
    stratify    = info[can_stratify == TRUE, var],
    numeric     = info[is_numeric == TRUE, var],
    event       = info[is_event == TRUE, var],
    all         = info$var
  )
}

#' What each variable selector may be set to, for one data set
#'
#' Every list is offered `"(none)"` first, and every selector opens on it. A
#' pre-filled box is a claim the app is in no position to make -- it does not
#' know which column the question is about -- and one that is easy to miss: a
#' figure of whichever column sorted first, drawn before anything was asked
#' for, looks exactly like a figure that was asked for.
#'
#' @param info Output of `gs_classify_vars()`.
#' @return A named list of character vectors, one per `selectInput` id.
#' @keywords internal
#' @noRd
gs_selector_choices <- function(info) {
  all_vars <- gs_vars_of(info, "all")
  continuous <- gs_vars_of(info, "continuous")
  # Fall back to every variable when nothing looks continuous, so the app is
  # still usable on oddly typed data.
  y_choices <- if (length(continuous)) continuous else all_vars

  out <- list(
    yvar = y_choices,
    xvar = all_vars,
    group = all_vars,
    # A subject identifier has as many levels as there are subjects, so it is
    # drawn from every variable rather than from the stratifying pool.
    idvar = all_vars,
    # The facet layer and the separate-figure layers come from the same pool:
    # a variable with few enough levels to be worth splitting on.
    facet = gs_vars_of(info, "stratify"),
    # survival::Surv() needs a numeric time and a two-valued event indicator.
    timevar = gs_vars_of(info, "numeric"),
    eventvar = gs_vars_of(info, "event"),
    # Categorising a variable that is already categorical would do nothing.
    cut_var = continuous
  )
  lapply(out, function(x) c(GS_NONE, x))
}

#' Choices for the layer selectors, labelled with level counts
#' @keywords internal
#' @noRd
gs_stratify_choices <- function(info) {
  vars <- gs_vars_of(info, "stratify")
  if (!length(vars)) return(character())
  sub <- info[var %in% vars]
  stats::setNames(sub$var, sprintf("%s  (%d)", sub$var, sub$n_levels))
}

#' The levels of a column, in the order they should be reported
#'
#' A factor is enumerated from `levels()`, so a declared level that matches no
#' rows is still visible; anything else from the values actually present.
#' `NA` is never a level: a row that does not say which stratum it belongs to
#' cannot be placed in one.
#' @keywords internal
#' @noRd
gs_levels_of <- function(col) {
  lv <- if (is.factor(col)) levels(col) else sort(unique(col[!is.na(col)]))
  as.character(lv)
}

#' Rows that carry a value for every layer variable
#' @keywords internal
#' @noRd
gs_complete_layers <- function(dt, vars) {
  vars <- intersect(vars, names(dt))
  if (!length(vars)) return(rep(TRUE, nrow(dt)))
  Reduce(`&`, lapply(vars, function(v) !is.na(dt[[v]])))
}

#' How many rows each layer variable is missing
#'
#' Reported rather than silently dropped: "n = 480" means something different
#' when 120 patients had no recorded site.
#'
#' @param dt A `data.table`.
#' @param vars Layer variables, see `gs_layer_vars()`.
#' @return A `data.table` with columns `var` and `n_missing`, only for the
#'   variables that actually have missing values.
#' @keywords internal
#' @noRd
gs_missing_report <- function(dt, vars) {
  vars <- intersect(vars, names(dt))
  out <- data.table::data.table(
    var = vars,
    n_missing = vapply(vars, function(v) sum(is.na(dt[[v]])), integer(1L),
                       USE.NAMES = FALSE)
  )
  out[n_missing > 0L]
}

#' The name of the stratum each row belongs to, in crossed mode
#'
#' `sex_Male__site_Site A`. The same construction is emitted into the
#' generated script, so the app and the script name their files identically.
#' @keywords internal
#' @noRd
gs_stratum_key <- function(dt, strat_vars) {
  parts <- lapply(strat_vars, function(v) paste0(v, "_", as.character(dt[[v]])))
  do.call(paste, c(parts, list(sep = "__")))
}

#' The human-readable label of the stratum each row belongs to
#'
#' `sex: Male | site: Site A`. Used for the Strata tab, the figure titles and
#' the facet strips alike, so that one stratum reads the same everywhere.
#' @keywords internal
#' @noRd
gs_stratum_label <- function(dt, strat_vars) {
  parts <- lapply(strat_vars,
                  function(v) sprintf("%s: %s", v, as.character(dt[[v]])))
  do.call(paste, c(parts, list(sep = " | ")))
}

#' One row per stratum
#'
#' In `"independent"` mode each stratifying variable is handled on its own, so
#' selecting `sex` and `treatment` yields `sex_M`, `sex_F`, `treatment_A`, ...
#' rather than their cross-product. In `"crossed"` mode one stratum is one
#' observed combination of all of them, which is what a third or fourth layer
#' of description means.
#'
#' Rows missing any stratifying variable are excluded before anything is
#' counted; `gs_missing_report()` is what tells the user about them.
#'
#' @param dt A `data.table`.
#' @param strat_vars Character vector of stratifying column names.
#' @param min_n Strata with fewer than this many rows are marked `keep = FALSE`.
#' @param mode `"independent"` or `"crossed"`.
#' @return A `data.table` with columns `var`, `level`, `label`, `file`, `n`,
#'   `keep`, `status`; zero rows when `strat_vars` is empty.
#' @keywords internal
#' @noRd
gs_strata_table <- function(dt, strat_vars, min_n = 0L,
                            mode = c("independent", "crossed")) {
  mode <- match.arg(mode)
  empty <- data.table::data.table(
    var = character(), level = character(), label = character(),
    file = character(), n = integer(), keep = logical(), status = character()
  )
  strat_vars <- intersect(strat_vars, names(dt))
  if (!length(strat_vars)) return(empty)
  min_n <- as.integer(min_n)
  if (length(min_n) != 1L || is.na(min_n)) min_n <- 0L
  min_n <- max(min_n, 0L)

  dt <- dt[gs_complete_layers(dt, strat_vars)]

  out <- if (mode == "crossed") {
    gs_strata_crossed(dt, strat_vars)
  } else {
    gs_strata_independent(dt, strat_vars)
  }

  if (!nrow(out)) return(empty)
  out[, keep := n > 0L & n >= min_n]

  # Ordered so that the n == 0 message wins over the min_n message.
  out[, status := "exported"]
  out[n < min_n, status := sprintf("skipped: n < %d", min_n)]
  out[n == 0L, status := "skipped: n = 0, no figure"]

  data.table::setcolorder(
    out, c("var", "level", "label", "file", "n", "keep", "status"))
  out[]
}

#' One stratum per level of each variable, taken one variable at a time
#' @keywords internal
#' @noRd
gs_strata_independent <- function(dt, strat_vars) {
  out <- data.table::rbindlist(lapply(strat_vars, function(v) {
    all_levels <- gs_levels_of(dt[[v]])

    # .N by a single column is the cheapest way to get the sizes.
    counts <- dt[, .N, by = c(v)]
    data.table::setnames(counts, c("level", "n"))
    counts[, level := as.character(level)]

    n <- counts$n[match(all_levels, counts$level)]
    n[is.na(n)] <- 0L
    data.table::data.table(var = v, level = all_levels, n = as.integer(n))
  }), use.names = TRUE)

  if (!nrow(out)) return(out)
  out[, label := sprintf("%s: %s", var, level)]
  out[, file := gs_safe_name(sprintf("%s_%s", var, level))]
  out[]
}

#' One stratum per observed combination of every variable
#'
#' Only combinations that occur are listed. The full grid is what the
#' package's own documentation warns against -- five levels crossed with five
#' gives twenty-five mostly empty figures -- and how much of it is empty is
#' reported as a count instead, by `attr(x, "n_possible")`.
#' @keywords internal
#' @noRd
gs_strata_crossed <- function(dt, strat_vars) {
  counts <- dt[, .N, by = c(strat_vars)]
  if (!nrow(counts)) return(data.table::data.table())

  # Combinations are listed in the level order of the variables, outermost
  # first, so the list reads like the nesting it describes.
  keys <- lapply(strat_vars, function(v) gs_levels_of(dt[[v]]))
  ord <- do.call(order, lapply(seq_along(strat_vars), function(i) {
    match(as.character(counts[[strat_vars[i]]]), keys[[i]])
  }))
  counts <- counts[ord]

  values <- lapply(strat_vars, function(v) as.character(counts[[v]]))
  out <- data.table::data.table(
    var = paste(strat_vars, collapse = " x "),
    level = do.call(paste, c(values, list(sep = " | "))),
    label = gs_stratum_label(counts, strat_vars),
    file = gs_safe_name(gs_stratum_key(counts, strat_vars)),
    n = as.integer(counts$N)
  )
  # How many combinations the variables could have formed, so that the app can
  # say how much of the cross-product is empty.
  data.table::setattr(out, "n_possible", prod(vapply(keys, length, integer(1L))))
  out[]
}

#' Append the stratum size to a label, e.g. "sex: M (N = 30)"
#'
#' One spelling of the size, used by the figure titles, the facet strips of the
#' all-figures preview and the panel strips of a facetted figure alike, so that
#' a reader meets the same notation wherever a count appears.
#' @keywords internal
#' @noRd
gs_label_n <- function(label, n) sprintf("%s (N = %d)", label, as.integer(n))

#' Make a string safe to use as a file name
#' @keywords internal
#' @noRd
gs_safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "NA"
  x
}

#' Build the long data used by the all-strata facet preview
#'
#' In independent mode each stratifying variable contributes one copy of the
#' data, labelled `variable: level`, so that a single `facet_wrap()` shows
#' every stratum in one render pass. In crossed mode there is nothing to
#' duplicate: each row belongs to exactly one combination. Only the columns
#' actually used by the plot are carried over, which keeps the duplication
#' cheap even on wide data.
#'
#' @param dt A `data.table`.
#' @param strat_vars Stratifying column names.
#' @param plot_cols Columns referenced by the plot's aesthetics.
#' @param min_n Strata smaller than this are dropped.
#' @param mode `"independent"` or `"crossed"`.
#' @param label_n Whether the strip carries the stratum size, following the
#'   same switch as the figure titles.
#' @return A `data.table` with an extra `.strat_label` factor column.
#' @keywords internal
#' @noRd
gs_long_strata <- function(dt, strat_vars, plot_cols, min_n = 0L,
                           mode = c("independent", "crossed"),
                           label_n = TRUE) {
  mode <- match.arg(mode)
  strat_vars <- intersect(strat_vars, names(dt))
  plot_cols <- intersect(unique(plot_cols), names(dt))
  dt <- dt[gs_complete_layers(dt, strat_vars)]

  # Panels appear in the order the Strata tab lists them. Sorting the labels
  # as text would be wrong for a categorised variable: "[0.5,3.9]" sorts after
  # "(3.9,8.13]".
  panel_order <- gs_strata_table(dt, strat_vars, 0L, mode)$label

  narrow <- function(d) {
    if (length(plot_cols)) d[, c(plot_cols, ".strat_label"), with = FALSE] else d
  }

  long <- if (mode == "crossed") {
    d <- dt[, unique(c(plot_cols, strat_vars)), with = FALSE]
    d[, .strat_label := gs_stratum_label(d, strat_vars)]
    narrow(d)
  } else {
    parts <- lapply(strat_vars, function(v) {
      d <- dt[, unique(c(plot_cols, v)), with = FALSE]
      d[, .strat_label := sprintf("%s: %s", v, as.character(d[[v]]))]
      narrow(d)
    })
    data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  }
  if (!nrow(long)) return(long)
  if (min_n > 0L) {
    long <- long[, if (.N >= min_n) .SD, by = .strat_label]
  }
  if (!nrow(long)) return(long)

  # Facet strips carry the stratum size, so the preview answers "how many are
  # in this panel?" without a trip to the Strata tab. Strata with no rows
  # cannot appear here at all, which is why they are reported separately.
  sizes <- long[, .N, by = .strat_label]
  labels <- if (isTRUE(label_n)) {
    gs_label_n(sizes$.strat_label, sizes$N)
  } else {
    sizes$.strat_label
  }
  long[, .strat_label := factor(
    labels[match(.strat_label, sizes$.strat_label)],
    levels = labels[order(match(sizes$.strat_label, panel_order))]
  )]
  long[]
}

#' Subset the data to a single stratum
#'
#' `row` is one row of `gs_strata_table()`, which is what the preview's
#' dropdown selects, so the rows shown are the rows the table counted.
#' @keywords internal
#' @noRd
gs_one_stratum <- function(dt, spec, row) {
  strat <- intersect(spec$strat_vars, names(dt))
  if (!length(strat) || is.null(row) || !nrow(row)) return(dt)

  if (identical(spec$strat_mode, "crossed")) {
    return(dt[gs_stratum_label(dt, strat) == row$label[1L]])
  }
  v <- row$var[1L]
  if (!v %in% names(dt)) return(dt)
  col <- as.character(dt[[v]])
  dt[!is.na(col) & col == as.character(row$level[1L])]
}

#' Split the data into the named list the export iterates over
#'
#' Mirrors the `split()` + `setNames()` construction emitted by
#' `gs_code_script()`, so that the "Export all PNGs" button and the generated
#' script produce byte-for-byte the same set of files. Columns are narrowed
#' before the split, which is what keeps this cheap on wide data.
#'
#' `drop = FALSE` keeps unused factor levels as zero-row elements, so that an
#' empty stratum can be counted and reported rather than disappearing. Those
#' elements are removed here, after the caller has had a chance to see them.
#'
#' With no stratifying variables the result is a single unnamed element, which
#' `gs_file_stem()` turns into a plain prefix.
#'
#' @param dt A `data.table`.
#' @param spec A spec list.
#' @param drop_empty Whether to remove strata that fail the size rules.
#' @return A named list of `data.table`s.
#' @keywords internal
#' @noRd
gs_split_strata <- function(dt, spec, drop_empty = TRUE) {
  cols <- gs_spec_cols(spec)
  strat <- intersect(spec$strat_vars, names(dt))
  narrow <- function(d, extra = character()) {
    keep_cols <- unique(c(cols, extra))
    if (length(keep_cols)) d[, keep_cols, with = FALSE] else d
  }

  # A row that does not say which stratum it belongs to is not put in one.
  dt <- dt[gs_complete_layers(dt, gs_layer_vars(spec))]

  if (!length(strat)) {
    return(stats::setNames(list(narrow(dt)), ""))
  }

  if (identical(spec$strat_mode, "crossed")) {
    d <- narrow(dt, strat)
    d[, .gs_stratum := gs_stratum_key(d, strat)]
    out <- split(d, by = ".gs_stratum", sorted = TRUE, drop = TRUE,
                 keep.by = FALSE)
  } else {
    out <- unlist(lapply(strat, function(v) {
      parts <- split(narrow(dt, v), by = v, sorted = TRUE, drop = FALSE)
      stats::setNames(parts, sprintf("%s_%s", v, names(parts)))
    }), recursive = FALSE)
  }

  if (!drop_empty) return(out)

  min_n <- as.integer(spec$min_n)
  if (is.na(min_n)) min_n <- 0L
  sizes <- vapply(out, nrow, integer(1L))
  # n == 0 is dropped regardless of min_n: there is nothing to draw.
  out[sizes > 0L & sizes >= min_n]
}

#' File-name stems for exported figures
#'
#' Vectorised over `nm`, because the Strata tab lists every name at once and
#' has to list the names the export will actually write.
#'
#' An empty `nm` is the unstratified case -- one figure, drawn from everything
#' -- which has no stratum to be named after and falls back to the prefix.
#' @keywords internal
#' @noRd
gs_file_stem <- function(prefix, nm) {
  safe <- gs_safe_name(nm)
  out <- if (nzchar(prefix)) paste0(prefix, "_", safe) else safe
  out[!nzchar(nm)] <- if (nzchar(prefix)) prefix else "plot"
  out
}
