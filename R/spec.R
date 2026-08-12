# A "spec" is the single plain-list description of what the user asked for.
# The preview, the in-app PNG export and the generated R code are all derived
# from it, so there is exactly one place where plot semantics live.

# The plot types offered by ggplotgui, kept identical on purpose, plus the
# Kaplan-Meier curve, which ggplotgui does not offer and which descriptive
# work on clinical cohorts needs constantly.
GS_PLOT_TYPES <- c("Boxplot", "Density", "Dot + Error", "Dotplot",
                   "Histogram", "Kaplan-Meier curve", "Line", "Scatter",
                   "Violin")

# Spelled once each, because they are tested against all over the package.
GS_KM <- "Kaplan-Meier curve"
GS_LINE <- "Line"

GS_THEMES <- c(
  "bw"        = "theme_bw()",
  "classic"   = "theme_classic()",
  "dark"      = "theme_dark()",
  "grey"      = "theme_grey()",
  "light"     = "theme_light()",
  "line_draw" = "theme_linedraw()",
  "minimal"   = "theme_minimal()"
)

# RColorBrewer palette names, grouped as ggplotgui groups them. Written out
# rather than queried from RColorBrewer so that it is not a dependency; the
# palettes themselves are applied by ggplot2's scale_*_brewer().
GS_PALETTES <- list(
  "Default (ggplot2)" = "",
  Qualitative = as.list(c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
                          "Set1", "Set2", "Set3")),
  Diverging = as.list(c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                        "RdYlBu", "RdYlGn", "Spectral")),
  Sequential = as.list(c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
                         "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd",
                         "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
                         "YlOrBr", "YlOrRd"))
)

# Plot types that describe the distribution of a single continuous variable
# on the x axis; they have no y variable.
GS_XONLY_TYPES <- c("Density", "Histogram")

# Plot types that put one variable on each axis and need both.
GS_XY_TYPES <- c("Line", "Scatter")

# Plot types a LOWESS smoother can be laid over. Both draw y against x with
# nothing summarised in between, which is what a smoother has to have.
GS_SMOOTH_TYPES <- c("Line", "Scatter")

# The ways a continuous variable can be turned into a categorical one.
GS_CUT_METHODS <- c(
  "Quantiles (equal-sized groups)" = "quantile",
  "Equal-width bins"               = "equal",
  "Custom cut points"              = "breaks"
)

# The file formats a figure can be written in. PNG goes through ragg; SVG is a
# vector format, so a figure stays sharp at any size and can still be edited in
# Illustrator or Inkscape after the fact -- which is what a journal usually
# asks for.
GS_FORMATS <- c("PNG (raster)" = "png", "SVG (vector)" = "svg")

#' Build a spec with defaults filled in
#' @keywords internal
#' @noRd
gs_spec <- function(...) {
  defaults <- list(
    plot_type   = "Boxplot",
    x           = "",
    y           = "",
    time        = "",             # Kaplan-Meier curve: follow-up time
    event       = "",             # Kaplan-Meier curve: 1 = event, 0 = censored
    id          = "",             # Line: one line per level of this variable
    group       = "",
    # Whether the grouping variable is continuous, which decides between the
    # discrete and the gradient form of the palette scale. Set by the server
    # from the classified data; FALSE is the safe default for a spec built by
    # hand, because a discrete scale is what a categorical group needs.
    group_continuous = FALSE,
    facet       = "",             # the facet_wrap layer: one figure, many panels
    strat_vars  = character(),    # the outer layers: one figure each
    strat_mode  = "independent",  # "independent" | "crossed"
    cuts        = list(),         # categorised continuous variables
    min_n       = 10L,
    show_n      = TRUE,
    jitter      = FALSE,
    line_points = FALSE,          # Line: draw the observations as well
    smooth      = FALSE,          # LOWESS smoother, see GS_SMOOTH_TYPES
    smooth_se   = FALSE,
    smooth_span = 0.75,
    binwidth    = NA_real_,
    alpha       = 0.6,
    bw_adjust   = 1,
    km_ci       = FALSE,
    km_censor   = TRUE,
    km_ylim     = TRUE,
    km_risk     = FALSE,          # the number-at-risk table under the curve
    theme       = "theme_bw()",
    palette     = "",
    title       = "",
    lab_x       = "",
    lab_y       = "",
    lab_legend  = "",
    data_name   = "mydata",
    outdir      = "figures",
    prefix      = "",
    format      = "png",          # see GS_FORMATS
    width       = 7,
    height      = 5,
    dpi         = 300,
    sample_n    = NA_integer_
  )
  args <- list(...)
  # modifyList() merges two lists element by element, which is right for every
  # field except `cuts`, itself a list: it would try to merge the rules with
  # the default empty list and, finding no names to match, drop them.
  cuts <- if ("cuts" %in% names(args)) args$cuts else defaults$cuts
  args$cuts <- NULL

  spec <- utils::modifyList(defaults, args)
  spec$strat_vars <- as.character(spec$strat_vars)
  spec$cuts <- gs_as_cuts(cuts)
  gs_normalize_spec(spec)
}

# --- the layers --------------------------------------------------------------

# How the outer layers combine. Independent is the default because it is what
# descriptive work usually wants: "what does this look like within each level
# of each variable", not "of every combination".
GS_STRAT_MODES <- c(
  "Each variable separately" = "independent",
  "Every combination"        = "crossed"
)

#' Every variable that splits the data into panels or figures
#'
#' These are the "layers": the facet variable makes panels inside one figure,
#' the stratifying variables make separate figures. A row that is missing any
#' of them cannot be placed, which is why they are handled together.
#' @keywords internal
#' @noRd
gs_layer_vars <- function(spec) {
  v <- c(spec$strat_vars, spec$facet)
  unique(v[nzchar(v)])
}

#' How many layers a spec describes, the described variable included
#'
#' One layer is a plain figure, two are a facet_wrap, and beyond that the
#' extra variables become separate figures.
#' @keywords internal
#' @noRd
gs_n_layers <- function(spec) 1L + length(gs_layer_vars(spec))

#' Fold plot-type quirks into the spec once, so nothing downstream repeats them
#'
#' As in ggplotgui, density and histogram take their continuous variable from
#' the Y-variable selector and draw it on the x axis; there is no y variable.
#' The Y-variable always wins here, because the UI hides the X selector for
#' these plot types and its last value would otherwise linger: typically a
#' categorical column that `stat_bin()` cannot use.
#'
#' A Kaplan-Meier curve takes its axes from the time and event variables
#' instead, so any lingering X/Y selection is dropped for the same reason.
#'
#' The options that belong to one plot type -- the line ID, the markers, the
#' smoother -- are switched off for every other type, so that a control the
#' user cannot currently see can never reach the generated code.
#' @keywords internal
#' @noRd
gs_normalize_spec <- function(spec) {
  if (spec$plot_type %in% GS_XONLY_TYPES) {
    if (nzchar(spec$y)) spec$x <- spec$y
    spec$y <- ""
  }
  if (identical(spec$plot_type, GS_KM)) {
    spec$x <- ""
    spec$y <- ""
  } else {
    spec$km_risk <- FALSE
  }
  if (!identical(spec$plot_type, GS_LINE)) {
    spec$id <- ""
    spec$line_points <- FALSE
  }
  if (!spec$plot_type %in% GS_SMOOTH_TYPES) spec$smooth <- FALSE
  if (!spec$strat_mode %in% GS_STRAT_MODES) spec$strat_mode <- "independent"
  if (!spec$format %in% GS_FORMATS) spec$format <- "png"
  # Nothing is grouped, so nothing is on a continuous scale either.
  if (!nzchar(spec$group)) spec$group_continuous <- FALSE
  # A variable can be one layer or another, never both: faceting by it and
  # then splitting the figures by it too would leave one panel per figure.
  if (nzchar(spec$facet)) {
    spec$strat_vars <- setdiff(spec$strat_vars, spec$facet)
  }
  spec
}

#' Turn the Shiny inputs into a spec
#' @keywords internal
#' @noRd
gs_spec_from_input <- function(input) {
  none <- function(x) if (is.null(x) || identical(x, "") || identical(x, GS_NONE)) "" else x
  gs_spec(
    plot_type  = input$plot_type %||% "Boxplot",
    x          = none(input$xvar),
    y          = none(input$yvar),
    time       = none(input$timevar),
    event      = none(input$eventvar),
    id         = none(input$idvar),
    group      = none(input$group),
    facet      = none(input$facet),
    strat_vars = input$strat_vars %||% character(),
    strat_mode = input$strat_mode %||% "independent",
    # Clearing the numeric box yields NA, which must not poison the size rules.
    min_n      = as.integer(gs_num(input$min_n, 10)),
    # NULL before the checkbox has registered; its UI default is TRUE.
    show_n     = !identical(input$show_n, FALSE),
    jitter     = isTRUE(input$jitter),
    line_points = isTRUE(input$line_points),
    smooth     = isTRUE(input$smooth),
    smooth_se  = isTRUE(input$smooth_se),
    smooth_span = gs_num(input$smooth_span, 0.75),
    binwidth   = gs_num(input$binwidth),
    alpha      = gs_num(input$alpha, 0.6),
    bw_adjust  = gs_num(input$bw_adjust, 1),
    km_ci      = isTRUE(input$km_ci),
    # NULL before the checkboxes have registered; both default to TRUE.
    km_censor  = !identical(input$km_censor, FALSE),
    km_ylim    = !identical(input$km_ylim, FALSE),
    km_risk    = isTRUE(input$km_risk),
    theme      = input$theme %||% "theme_bw()",
    palette    = none(input$palette),
    title      = input$title %||% "",
    lab_x      = input$lab_x %||% "",
    lab_y      = input$lab_y %||% "",
    lab_legend = input$lab_legend %||% "",
    data_name  = input$data_name %||% "mydata",
    # A folder of nothing but spaces would pass nzchar() and then fail to be
    # created, so it is trimmed before it can reach dir.create().
    outdir     = if (nzchar(trimws(input$outdir %||% ""))) {
                   trimws(input$outdir)
                 } else {
                   "figures"
                 },
    prefix     = input$prefix %||% "",
    format     = input$format %||% "png",
    width      = gs_num(input$width, 7),
    height     = gs_num(input$height, 5),
    dpi        = gs_num(input$dpi, 300)
  )
}

#' Columns a spec actually reads from the data
#' @keywords internal
#' @noRd
gs_spec_cols <- function(spec) {
  # `id` is blank for every type but Line, so it needs no test of its own.
  cols <- c(spec$x, spec$y, spec$group, spec$facet, spec$id)
  if (identical(spec$plot_type, GS_KM)) cols <- c(spec$time, spec$event, cols)
  unique(cols[nzchar(cols)])
}

#' Check that a spec can be drawn; returns a character vector of problems
#' @keywords internal
#' @noRd
gs_validate_spec <- function(spec, info = NULL) {
  problems <- character()
  type <- spec$plot_type

  # A selected variable can vanish under the app's feet: removing a
  # categorisation removes its column. Saying so beats letting the plot code
  # fail on a missing object, and nothing else is worth reporting until the
  # selection is valid again.
  if (!is.null(info) && nrow(info)) {
    gone <- setdiff(gs_spec_cols(spec), gs_vars_of(info, "all"))
    if (length(gone)) {
      return(sprintf("'%s' is no longer a column in the data.", gone))
    }
  }

  if (type == GS_KM) {
    if (!nzchar(spec$time)) {
      problems <- c(problems, sprintf("A %s needs a time variable.", GS_KM))
    }
    if (!nzchar(spec$event)) {
      problems <- c(problems, sprintf(
        "A %s needs an event variable (1 = event, 0 = censored).", GS_KM))
    }
    if (!is.null(info) && nrow(info)) {
      problems <- c(problems, gs_validate_km_vars(spec, info))
    }
    return(problems)
  }

  if (type %in% GS_XONLY_TYPES) {
    if (!nzchar(spec$x)) {
      problems <- c(problems, sprintf(
        "%s needs a continuous variable (choose one under Y-variable).", type))
    }
  } else if (type %in% GS_XY_TYPES) {
    if (!nzchar(spec$x) || !nzchar(spec$y)) {
      problems <- c(problems,
                    sprintf("%s needs both an X- and a Y-variable.", type))
    }
  } else {
    if (!nzchar(spec$y)) {
      problems <- c(problems, sprintf("%s needs a Y-variable (continuous).", type))
    }
  }

  if (!is.null(info) && nrow(info)) {
    continuous <- gs_vars_of(info, "continuous")
    needs_cont <- if (type %in% GS_XONLY_TYPES) spec$x else spec$y
    if (nzchar(needs_cont) && !needs_cont %in% continuous) {
      problems <- c(problems, sprintf(
        "'%s' does not look continuous; %s expects a continuous variable there.",
        needs_cont, type))
    }
    # Only Scatter is held to a continuous X. A line plot's X is time, which
    # is as often a date or a visit number -- neither of which is classified
    # as continuous -- as it is a measurement.
    if (type == "Scatter" && nzchar(spec$x) && !spec$x %in% continuous) {
      problems <- c(problems,
                    sprintf("Scatter expects a continuous X-variable; '%s' is not.", spec$x))
    }
  }

  problems
}

#' The type checks that only a Kaplan-Meier curve needs
#'
#' `survival::Surv()` refuses a non-numeric time and an event that is not
#' 0/1, 1/2 or logical, so both are checked here rather than left to surface
#' as an error from inside the fit.
#' @keywords internal
#' @noRd
gs_validate_km_vars <- function(spec, info) {
  problems <- character()
  numeric_vars <- gs_vars_of(info, "numeric")
  event_vars <- gs_vars_of(info, "event")

  if (nzchar(spec$time) && !spec$time %in% numeric_vars) {
    problems <- c(problems, sprintf(
      "The time variable must be numeric; '%s' is not.", spec$time))
  }
  if (nzchar(spec$event) && !spec$event %in% event_vars) {
    problems <- c(problems, sprintf(
      paste("The event variable must be 0/1, 1/2 or TRUE/FALSE; '%s' is not.",
            "Recode it before plotting."),
      spec$event))
  }
  problems
}

# --- small helpers -----------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

GS_NONE <- "(none)"

#' Coerce an input to a finite number, falling back to a default
#' @keywords internal
#' @noRd
gs_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) != 1L) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) default else x
}
