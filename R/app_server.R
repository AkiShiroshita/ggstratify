# Server logic. The expensive things -- reading the data and classifying its
# columns -- happen once per data set and are stored in reactiveVal()s. Every
# other reactive is cheap, and the preview render is cached on the spec so that
# flipping back to a previous setting is instant.

#' Build the ggstratify server function
#'
#' The data set is fixed for the life of the app: it is the object the launcher
#' was called with, and there is no way to load another one from inside. That
#' is deliberate -- see the *Before you start* section of [ggstratify()] -- and
#' it is what lets `raw` be an ordinary value rather than a reactive.
#'
#' @param dataset The data supplied to the launcher.
#' @param data_name What the generated code should call it.
#' @keywords internal
#' @noRd
gs_server <- function(dataset, data_name = "mydata") {
  # Coerced once, outside the session, so that every session of a hosted app
  # shares the work rather than repeating it.
  raw <- gs_prepare_data(dataset)

  function(input, output, session) {

    # Categorization rules, in the order the user added them.
    cuts <- shiny::reactiveVal(list())

    # The name the data was passed under, rather than the placeholder the
    # control starts with, so the code on the R-code tab runs as it is read.
    if (!identical(data_name, "mydata")) {
      shiny::updateTextInput(session, "data_name", value = data_name)
    }

    # Rules that can actually be applied to the data in hand.
    cuts_used <- shiny::reactive(gs_valid_cuts(cuts(), names(raw)))

    # What the whole app works on: the data plus the categorized columns. They
    # are built by evaluating the very lines the R-code tab shows, so the app
    # and the script cannot disagree about them.
    dat <- shiny::reactive(gs_apply_cuts(raw, cuts_used()))

    # Re-derived whenever a categorized column appears or disappears.
    info <- shiny::reactive(gs_classify_vars(dat()))

    # --- populate the selectors once per data set ----------------------------

    # Adding a categorized variable repopulates every selector, so a selector
    # keeps what the user picked whenever that choice still exists. Only a
    # selection that has become impossible falls back to "(none)", which is
    # also where every selector starts -- see gs_selector_choices().
    keep_or <- function(id, choices) {
      cur <- input[[id]]
      if (!is.null(cur) && length(cur) == 1L && cur %in% choices) cur else GS_NONE
    }

    # The Derive-a-variable selector offers a different pool for "missing vs
    # observed" than for the methods that cut values, so the method is a
    # second reason to repopulate. Everything else keeps its selection through
    # keep_or(); only a choice the new pool no longer contains -- a factor
    # picked under "missing", after a switch back to quantiles -- falls back.
    shiny::observeEvent(list(info(), input$cut_method), {
      nfo <- info()
      method <- input$cut_method %||% "quantile"
      choices <- gs_selector_choices(nfo, method)
      for (id in names(choices)) {
        shiny::updateSelectInput(session, id, choices = choices[[id]],
                                 selected = keep_or(id, choices[[id]]),
                                 label = if (identical(id, "cut_var")) {
                                   gs_cut_var_label(method)
                                 })
      }
      shiny::updateCheckboxGroupInput(
        session, "strat_vars", choices = gs_stratify_choices(nfo),
        selected = intersect(input$strat_vars %||% character(),
                             gs_vars_of(nfo, "stratify")))
    })

    # --- categorizing --------------------------------------------------------

    # A name that does not collide with the data, refreshed as the source
    # variable changes but never overwriting something the user typed.
    shiny::observeEvent(list(input$cut_var, input$cut_method, cuts_used()), {
      dt <- dat()
      shiny::req(input$cut_var)
      # Nothing chosen yet: there is no name to suggest.
      if (identical(input$cut_var, GS_NONE)) return()
      method <- input$cut_method %||% "quantile"
      suggested <- gs_cut_name(input$cut_var, names(dt), method)
      current <- input$cut_name %||% ""
      if (!nzchar(current) || current %in% names(dt) ||
          grepl(gs_suggested_name_pattern(), current)) {
        shiny::updateTextInput(session, "cut_name", value = suggested)
      }
    })

    shiny::observeEvent(input$add_cut, {
      dt <- dat()
      shiny::req(input$cut_var)
      if (identical(input$cut_var, GS_NONE)) {
        shiny::showNotification(
          if (identical(input$cut_method, "missing")) {
            "Choose the variable whose missing values you want to group by."
          } else {
            "Choose the continuous variable to categorize."
          },
          type = "warning")
        return()
      }

      breaks <- gs_parse_breaks(input$cut_points)
      if (identical(input$cut_method, "breaks") && anyNA(breaks)) {
        shiny::showNotification(
          "Cut points must be numbers, separated by commas.",
          type = "error", duration = 8)
        return()
      }

      cut <- gs_cut(var = input$cut_var, new = trimws(input$cut_name %||% ""),
                    method = input$cut_method %||% "quantile",
                    n = input$cut_n, breaks = breaks)
      probs <- gs_check_cut(dt, cut)
      if (length(probs)) {
        shiny::showNotification(paste(probs, collapse = " "), type = "error",
                                duration = 10)
        return()
      }

      cuts(c(cuts(), list(cut)))
      shiny::showNotification(sprintf("Added %s", gs_cut_describe(cut)),
                              type = "message", duration = 5)
    })

    shiny::observeEvent(input$remove_cut, {
      cs <- cuts()
      idx <- suppressWarnings(as.integer(input$cut_remove))
      if (is.na(idx) || idx < 1L || idx > length(cs)) return()
      cuts(cs[-idx])
    })

    output$cuts_list <- shiny::renderUI({
      cs <- cuts_used()
      if (!length(cs)) {
        return(shiny::helpText("No derived variables yet."))
      }
      labels <- vapply(cs, gs_cut_describe, character(1L))
      shiny::div(
        class = "mt-3",
        shiny::selectInput("cut_remove", "Derived variables",
                           choices = stats::setNames(seq_along(cs), labels)),
        shiny::actionButton("remove_cut", "Remove", class = "btn-outline-danger btn-sm",
                            icon = shiny::icon("trash"))
      )
    })

    # --- spec ---------------------------------------------------------------

    spec_r <- shiny::debounce(
      shiny::reactive({
        spec <- gs_spec_from_input(input)
        # The categorization rules are held outside the inputs, because they
        # accumulate rather than being read off a control.
        spec$cuts <- cuts_used()
        # Which palette scale the group needs is a property of the data, not
        # of any control, so it is filled in here rather than read off one.
        nfo <- info()
        spec$group_continuous <- nzchar(spec$group) && !is.null(nfo) &&
          spec$group %in% gs_vars_of(nfo, "continuous")
        # Removing a categorization removes its column, and the browser takes
        # a round trip to notice. Until it does, the ticked variable would
        # otherwise reach the generated script, which would then not run.
        dt <- dat()
        spec$strat_vars <- intersect(spec$strat_vars, names(dt))
        if (!spec$facet %in% names(dt)) spec$facet <- ""
        gs_normalize_spec(spec)
      }),
      millis = 300
    )

    problems <- shiny::reactive(gs_validate_spec(spec_r(), info()))

    # --- the layers ----------------------------------------------------------

    # Every figure is drawn from rows that say which figure they belong to.
    # Excluding them here, once, is what keeps the n on the Strata tab, the n
    # in the title and the rows in the figure the same number.
    layer_data <- shiny::reactive({
      dt <- dat()
      dt[gs_complete_layers(dt, gs_layer_vars(spec_r()))]
    })

    missing_report <- shiny::reactive({
      gs_missing_report(dat(), gs_layer_vars(spec_r()))
    })

    output$layer_summary <- shiny::renderUI({
      spec <- spec_r()
      n <- gs_n_layers(spec)
      described <- if (identical(spec$plot_type, GS_KM)) {
        if (nzchar(spec$time)) spec$time else "the outcome"
      } else if (nzchar(spec$y)) spec$y else if (nzchar(spec$x)) spec$x else "nothing yet"
      parts <- sprintf("%d layer%s: describing %s", n, if (n == 1L) "" else "s",
                       described)
      if (nzchar(spec$facet)) {
        parts <- c(parts, sprintf("panelled by %s", spec$facet))
      }
      st <- if (length(spec$strat_vars)) strata() else NULL
      if (!is.null(st) && nrow(st)) {
        parts <- c(parts, sprintf("%d figure%s from %s", sum(st$keep),
                                  if (sum(st$keep) == 1L) "" else "s",
                                  paste(spec$strat_vars, collapse =
                                          if (identical(spec$strat_mode, "crossed"))
                                            " x " else ", ")))
      }
      shiny::tagList(
        shiny::div(class = "alert alert-light border py-2 px-3 small mt-2",
                   paste(parts, collapse = ", ")),
        gs_self_missing_alert(gs_self_missing_cuts(spec))
      )
    })

    # --- strata -------------------------------------------------------------

    strata <- shiny::reactive({
      spec <- spec_r()
      gs_strata_table(layer_data(), spec$strat_vars, spec$min_n, spec$strat_mode)
    })

    # updateSelectInput() moves the box back to its first choice, and strata()
    # is invalidated by every control in the sidebar -- an opacity, a theme --
    # not only by the ones that decide which figures there are. Pushing the
    # list unconditionally would therefore send the preview back to the first
    # figure whenever anything at all was adjusted. So it is pushed only when
    # it is a different list, and the figure on screen is followed to its new
    # position rather than dropped: it is the same figure, further down.
    shown_strata <- NULL
    shiny::observeEvent(strata(), {
      st <- strata()[keep == TRUE]
      choices <- if (nrow(st)) {
        stats::setNames(as.character(seq_len(nrow(st))), st$label)
      } else {
        character()
      }
      if (identical(choices, shown_strata)) return()
      shown <- names(shown_strata)[match(input$preview_stratum, shown_strata)]
      shown_strata <<- choices
      keep_showing <- length(shown) == 1L && !is.na(shown) &&
        shown %in% names(choices)
      shiny::updateSelectInput(
        session, "preview_stratum", choices = choices,
        selected = if (keep_showing) choices[[shown]] else NULL)
    })

    # The one figure the app is currently talking about: the one the preview
    # draws when the figures are shown one at a time, and the one the R-code
    # tab writes the code for. `NULL` when the layers make a single figure.
    current_stratum <- shiny::reactive({
      spec <- spec_r()
      if (!length(spec$strat_vars)) return(NULL)
      st <- strata()[keep == TRUE]
      if (!nrow(st)) return(NULL)
      idx <- suppressWarnings(as.integer(input$preview_stratum))
      if (is.na(idx) || idx < 1L || idx > nrow(st)) idx <- 1L
      st[idx]
    })

    # --- preview ------------------------------------------------------------

    # Showing every figure at once borrows the facet machinery, so both modes
    # are possible only when the figures are split and no facet variable has
    # claimed the panels already.
    preview_choice <- shiny::reactive({
      spec <- spec_r()
      length(spec$strat_vars) > 0 && !nzchar(spec$facet)
    })

    # The chosen variables decide what the preview can show, so they set it:
    # every figure at once as soon as that becomes possible, one at a time as
    # soon as it stops being. The switch is hidden in the second case, which
    # is why this needs no note on screen. A choice the user makes while both
    # modes are open is left alone, which is what the comparison is for:
    # observeEvent() fires whenever its expression is invalidated, and
    # preview_choice() reads the spec, so without it every change of opacity
    # would push the preview back to showing all the figures at once.
    shown_choice <- NULL
    shiny::observeEvent(preview_choice(), {
      if (identical(preview_choice(), shown_choice)) return()
      shown_choice <<- preview_choice()
      shiny::updateRadioButtons(
        session, "preview_mode",
        selected = if (preview_choice()) "facet" else "single")
    })

    facet_preview <- shiny::reactive({
      identical(input$preview_mode, "facet") && preview_choice()
    })

    output$preview_note <- shiny::renderUI({
      spec <- spec_r()
      thr <- gs_sample_threshold(spec)
      if (!isTRUE(input$sample_big) || nrow(dat()) <= thr) return(NULL)
      msg <- sprintf(
        paste("Preview drawn from a random sample of about %s rows%s.",
              "The exported figures and the generated code always use every",
              "row%s."),
        format(thr, big.mark = ","),
        # Trajectories are sampled whole: half a line is a different line.
        if (nzchar(spec$id)) sprintf(", taken one whole %s at a time", spec$id) else "",
        # The strips are counted from what is drawn, which here is the sample.
        # The Strata tab and the exported figures still count every row.
        if (gs_uses_facet_n(spec)) {
          ", including the N on each panel strip"
        } else {
          ""
        }
      )
      shiny::div(class = "alert alert-warning py-2 px-3 small", msg)
    })

    output$plot <- shiny::renderPlot({
      dt <- layer_data()
      probs <- problems()
      shiny::validate(shiny::need(!length(probs), paste(probs, collapse = "\n")))

      spec <- spec_r()
      cols <- gs_spec_cols(spec)

      if (facet_preview()) {
        long <- gs_long_strata(dt, spec$strat_vars, cols, spec$min_n,
                               spec$strat_mode, label_n = spec$show_n)
        shiny::validate(shiny::need(
          nrow(long) > 0,
          "No stratum reaches the minimum n. Lower it, or pick another variable."
        ))
        long <- gs_downsample(long, spec, isTRUE(input$sample_big),
                              by = ".strat_label")
        gs_eval_plot(gs_code_figure(spec, "d", facet_strata = TRUE), long)
      } else {
        row <- current_stratum()
        d <- dt
        title <- NULL
        if (!is.null(row)) {
          d <- gs_one_stratum(dt, spec, row)
          # n comes from the strata table, so it reports the true stratum size
          # even when the preview itself is drawn from a subsample.
          title <- gs_title_literal(spec, row$file[1L], row$n[1L])
        }
        d <- if (length(cols)) d[, cols, with = FALSE] else d
        d <- gs_downsample(d, spec, isTRUE(input$sample_big))
        gs_eval_plot(gs_code_figure(spec, "d", title_expr = title), d)
      }
    }, res = GS_PREVIEW_RES) |>
      # The data cannot change under the app, so the spec -- which carries the
      # categorization rules -- plus the preview controls is the whole key.
      shiny::bindCache(spec_r(), input$preview_mode, input$preview_stratum,
                       input$sample_big)

    # --- tables -------------------------------------------------------------

    output$data_table <- shiny::renderTable({
      utils::head(dat(), 200L)
    }, striped = TRUE, spacing = "xs", rownames = FALSE, digits = 4)

    # A one-line count of what will and will not be drawn, so the n = 0 case
    # and the excluded rows are visible without reading the whole table.
    output$strata_summary <- shiny::renderUI({
      st <- strata()
      miss <- missing_report()
      if (!nrow(st) && !nrow(miss)) return(NULL)

      parts <- character()
      if (nrow(st)) {
        n_empty <- sum(st$n == 0L)
        n_small <- sum(!st$keep & st$n > 0L)
        parts <- sprintf("%d strata, %d exported", nrow(st), sum(st$keep))
        if (n_small) parts <- c(parts, sprintf("%d below the minimum N", n_small))
        if (n_empty) parts <- c(parts, sprintf("%d with N = 0 (no figure)", n_empty))
        possible <- attr(st, "n_possible")
        if (!is.null(possible) && possible > nrow(st)) {
          parts <- c(parts, sprintf("%d of %d possible combinations occur",
                                    nrow(st), possible))
        }
      }
      if (nrow(miss)) {
        parts <- c(parts, sprintf(
          "excluded rows with a missing %s",
          paste(sprintf("%s (%d)", miss$var, miss$n_missing), collapse = ", ")))
      }
      shiny::div(
        class = if (nrow(miss) || !nrow(st) || any(!st$keep))
                  "alert alert-info py-2 px-3 small"
                else "alert alert-success py-2 px-3 small",
        paste(parts, collapse = " | ")
      )
    })

    # Whether a stratum is drawn is already said by its file name -- a stratum
    # with no file gets no figure -- and why it is not is said once, in the
    # summary line above. A status column would repeat both, row by row.
    output$strata_table <- shiny::renderTable({
      st <- strata()
      shiny::validate(shiny::need(
        nrow(st) > 0,
        "Tick at least one variable under 'Separate figures' in the sidebar."
      ))
      spec <- spec_r()
      # The same stem and the same extension the export will use, so that the
      # name listed here is the name that appears on disk.
      names <- paste0(gs_file_stem(gs_export_prefix(spec), st$file),
                      ".", spec$format)
      data.frame(
        Variable = st$var,
        Level = st$level,
        N = st$n,
        `File name` = ifelse(st$keep, names, "-"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, spacing = "xs", rownames = FALSE, digits = 0)

    # --- generated code -----------------------------------------------------

    output$code <- shiny::renderText({
      probs <- problems()
      if (length(probs)) {
        return(paste0("# Cannot generate code yet:\n# - ",
                      paste(probs, collapse = "\n# - ")))
      }
      # The code is for one figure, so it has to say which one when the Plot
      # tab is showing several at once.
      note <- if (facet_preview()) {
        paste("The Plot tab is showing every figure at once;",
              "this is the code for one of them.")
      } else {
        NULL
      }
      gs_code_script(spec_r(), current_stratum(), note)
    })

    # --- export -------------------------------------------------------------

    shiny::observeEvent(input$export, {
      dt <- layer_data()
      probs <- problems()
      if (length(probs)) {
        shiny::showNotification(paste(probs, collapse = " "), type = "error",
                                duration = 10)
        return()
      }

      spec <- spec_r()
      outdir <- spec$outdir
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      if (!dir.exists(outdir)) {
        shiny::showNotification(paste("Could not create folder:", outdir),
                                type = "error", duration = 10)
        return()
      }

      prefix <- gs_export_prefix(spec)
      dt_list <- gs_split_strata(dt, spec)
      st <- strata()
      n_skipped <- if (nrow(st)) sum(!st$keep) else 0L

      if (!length(dt_list)) {
        shiny::showNotification(
          "Every stratum is empty or below the minimum n; nothing was written.",
          type = "warning", duration = 8
        )
        return()
      }

      device <- gs_device(spec$format)
      ext <- paste0(".", spec$format)
      step <- 1 / length(dt_list)
      written <- 0L
      shiny::withProgress(message = "Writing figures", value = 0, {
        # Every figure is drawn by evaluating the code generator's output, the
        # same way the preview and the R-code tab are, so the file on disk and
        # the code on screen cannot drift apart.
        Map(function(d, nm) {
          stem <- gs_file_stem(prefix, nm)
          title <- if (nzchar(nm)) {
            gs_title_literal(spec, gs_safe_name(nm), nrow(d))
          } else {
            NULL
          }
          p <- gs_eval_plot(gs_code_figure(spec, "d", title_expr = title), d)
          ggplot2::ggsave(file.path(outdir, paste0(stem, ext)), plot = p,
                          width = spec$width, height = spec$height,
                          dpi = spec$dpi, device = device)
          written <<- written + 1L
          shiny::incProgress(step, detail = paste0(stem, ext))
          NULL
        }, dt_list, names(dt_list))
      })

      msg <- sprintf("Wrote %d %s file%s to %s", written,
                     toupper(spec$format),
                     if (written == 1L) "" else "s",
                     normalizePath(outdir, winslash = "/", mustWork = FALSE))
      if (n_skipped > 0L) {
        msg <- paste0(msg, sprintf("; %d stratum/strata skipped (see the Strata tab)",
                                   n_skipped))
      }
      shiny::showNotification(msg, type = "message", duration = 8)
    })
  }
}

#' The graphics device one of `GS_FORMATS` is written through
#'
#' PNG goes through \pkg{ragg}, which is both faster and better at finding a
#' font than the default device. SVG is a vector format and needs a vector
#' device: \pkg{svglite} when it is installed, and otherwise the cairo device
#' that ships with R, so that the option works without a further install.
#'
#' @param format `"png"` or `"svg"`. Anything else is an error.
#' @return A device function, ready to hand to [ggplot2::ggsave()].
#' @keywords internal
#' @noRd
gs_device <- function(format = "png") {
  if (identical(format, "png")) return(ragg::agg_png)
  # Anything but the two formats the UI offers is a bug on the way in, and is
  # worth saying so rather than quietly writing a PNG under an .svg name.
  if (!identical(format, "svg")) {
    stop("Unknown figure format: ", format, call. = FALSE)
  }
  if (requireNamespace("svglite", quietly = TRUE)) {
    return(svglite::svglite)
  }
  grDevices::svg
}

# Nominal resolution the preview is drawn at. shiny::renderPlot() defaults to
# 72, at which a 620-pixel-tall panel is treated as an 8.6-inch figure and
# ggplot2's 11-point text comes out looking tiny beside it. Raising the
# resolution shrinks the notional figure instead, so the same theme text fills
# the panel the way it fills the exported figure. A browser window wide enough
# to hold the sidebar and a 900-pixel plot puts the notional figure at about
# the 7 inches the export defaults to, which is what makes the two look alike.
# The plot code is untouched by this, so the preview still matches the R-code
# tab exactly.
GS_PREVIEW_RES <- 150

# Row counts above which a preview is drawn from a sample. Scatter and Line
# need a much lower cap: every row becomes its own mark or line segment, so
# they cost roughly 20x what a boxplot of the same data costs.
#
# Both caps are set high on purpose. Sampling is a compromise -- it is the one
# place where what is on screen is not what the data says -- so it should be
# reached by data that genuinely will not draw, not by data that is merely
# large. A boxplot, histogram or density summarises its rows before drawing
# anything, so five million of them is a few seconds of arithmetic; the caps
# below are where the wait stops being worth it, not where it starts.
GS_SAMPLE_SCATTER <- 1000000L
GS_SAMPLE_OTHER <- 10000000L

#' Row target for previewing a spec's figure
#'
#' A target rather than a hard cap: `gs_downsample()` will go over it to keep
#' a whole trajectory or a whole stratum, see there.
#'
#' A Kaplan-Meier curve is never sampled. The other plot types show the shape
#' of a distribution, which a large sample preserves; a survival curve *is*
#' the estimate, and a sampled one would be a different, wider curve shown
#' without saying so.
#' @keywords internal
#' @noRd
gs_sample_threshold <- function(spec) {
  type <- spec$plot_type
  if (identical(type, GS_KM)) return(Inf)
  if (type %in% c("Scatter", GS_LINE)) GS_SAMPLE_SCATTER else GS_SAMPLE_OTHER
}

#' Thin a data.table so the preview stays responsive
#'
#' Applies to previews only. The in-app export and the generated script always
#' use every row, so nothing written to disk is ever silently subsampled.
#'
#' `threshold` is a target, not a strict upper bound, and both of the grouped
#' branches can exceed it rather than drop a group entirely:
#'
#'   * A line plot with an ID variable is sampled one whole trajectory at a
#'     time -- dropping rows from within a line would draw a line nobody has,
#'     in the same way that a sampled survival curve is a different curve --
#'     so the rows returned are those of the sampled IDs, which is only
#'     `threshold` on average.
#'   * When `by` is given, sampling is done within each group so that small
#'     strata keep their rows instead of being washed out by large ones, and
#'     every group keeps at least one row. With more groups than `threshold`
#'     that alone puts the result over it -- which is the intended trade: a
#'     panel drawn from no rows at all is worse than a preview slightly larger
#'     than asked for. In practice the caps are in the millions and the panels
#'     in the tens, so the case is a corner one.
#'
#' The ungrouped branch is exact.
#'
#' @param d A `data.table`.
#' @param spec A spec list, which sets the sampling unit and, by default, the
#'   row target.
#' @param enabled Whether the user left subsampling switched on.
#' @param by Optional grouping column to sample within.
#' @param threshold Row target. Defaults to the one the spec's plot type asks
#'   for; passed explicitly by the tests, which would otherwise have to
#'   allocate a table of the real cap's size to reach this code at all.
#' @return `d`, or a sample of it.
#' @keywords internal
#' @noRd
gs_downsample <- function(d, spec, enabled = TRUE, by = NULL,
                          threshold = gs_sample_threshold(spec)) {
  if (!isTRUE(enabled) || !nrow(d)) return(d)
  if (nrow(d) <= threshold) return(d)

  # Blank for every plot type but Line, see gs_normalize_spec().
  id <- spec$id %||% ""
  if (nzchar(id) && id %in% names(d)) {
    ids <- unique(d[[id]])
    # The fraction first: two integer row counts multiplied together overflow.
    n_keep <- max(1L, as.integer(floor(length(ids) * (threshold / nrow(d)))))
    # Computed outside the `[` so that `id`, the local holding the column's
    # name, cannot be read as the column of that name instead.
    .gs_keep <- d[[id]] %in% sample(ids, n_keep)
    return(d[.gs_keep])
  }

  if (!is.null(by) && by %in% names(d)) {
    per_group <- max(1L, threshold %/% max(data.table::uniqueN(d[[by]]), 1L))
    return(d[, if (.N > per_group) .SD[sample.int(.N, per_group)] else .SD,
             by = c(by)])
  }
  d[sample.int(nrow(d), threshold)]
}
