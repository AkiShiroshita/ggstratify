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
GS_DOT <- "Dot + Error"

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
# The shape is deliberately not uniform: selectInput() reads a bare string as
# one option and a list as an <optgroup>, so wrapping the default in list()
# would turn it into a group of one nameless entry rather than the plain first
# choice it has to be.
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

# What the bar through a Dot + Error point stands for.
#
# The standard error is the default because it is what the figure has always
# drawn, and because it is a statement about the mean rather than about where a
# further observation would fall. A confidence interval answers the question
# people usually mean to ask, so it is offered beside it.
#
# The two proportion intervals exist because a Wald interval on a proportion is
# wrong in exactly the cases that matter: near 0 or 1 it runs outside the range
# a proportion can take, and when nobody had the outcome it has zero width and
# claims certainty. Clopper-Pearson inverts the binomial test and so never
# under-covers; Wilson inverts the score test and is the better-centred of the
# two at small n. Which to prefer is a judgement, so both are offered.
GS_ERR_TYPES <- c(
  "Standard error of the mean"                = "se",
  "Confidence interval (mean)"                = "normal",
  "Confidence interval (proportion, exact)"   = "exact",
  "Confidence interval (proportion, Wilson)"  = "wilson"
)

# The intervals that describe a proportion, and so need a Y variable coded as
# 0 and 1 rather than a measurement.
GS_ERR_PROP_TYPES <- c("exact", "wilson")

# The interval types that carry a confidence level. A standard error does not:
# it is one standard error, not a coverage statement.
GS_ERR_CI_TYPES <- c("normal", "exact", "wilson")

GS_ERR_LEVELS <- c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99)

# Tick label angles. Twelve month names or a set of long factor levels do not
# fit side by side along an axis, and ggplot2 draws them overlapping rather
# than dropping any, so turning them is the way out.
GS_TICK_ANGLES <- c("Horizontal" = "0", "30 degrees" = "30",
                    "45 degrees" = "45", "60 degrees" = "60",
                    "Vertical" = "90")

# Plot types that describe the distribution of a single continuous variable
# on the x axis; they have no y variable.
GS_XONLY_TYPES <- c("Density", "Histogram")

# Plot types that put one variable on each axis and need both.
GS_XY_TYPES <- c("Line", "Scatter")

# Plot types a LOWESS smoother can be laid over. Both draw y against x with
# nothing summarised in between, which is what a smoother has to have.
GS_SMOOTH_TYPES <- c("Line", "Scatter")

# Plot types that read a survey weight through ggplot2's own `weight`
# aesthetic: a bin, a density or a box is a summary of the rows, and a weighted
# summary is the same summary with each row counted as many times as its
# weight. Dot + Error and the Kaplan-Meier curve are weighted too, but through
# the survey package, because their bars and bands are design-based intervals
# rather than summaries; a Scatter or Line figure is weighted only in its
# smoother, since a point is one row whatever its weight.
GS_WEIGHT_AES_TYPES <- c("Boxplot", "Density", "Histogram", "Violin")

# The one type that cannot carry a weight at all. A dotplot draws one dot per
# row, and ggplot2 refuses a weight that is not a whole number rather than
# drawing a fraction of a dot.
GS_UNWEIGHTED_TYPES <- "Dotplot"

# The ways a column can be turned into another one. The first three read a
# continuous variable's values; the fourth reads only whether there is a value
# at all, so it applies to a column of any type; the fifth reads a date or a
# time and asks at what resolution to look at it.
GS_CUT_METHODS <- c(
  "Quantiles (equal-sized groups)" = "quantile",
  "Equal-width bins"               = "equal",
  "Custom cut points"              = "breaks",
  "Missing vs observed"            = "missing",
  "Time resolution"                = "period"
)

# The methods that read values rather than the absence of them, and so need a
# numeric column. Named once here because the UI, the validator and the
# selector list all have to agree on it.
GS_VALUE_CUT_METHODS <- c("quantile", "equal", "breaks")

# The methods that ask for a number of groups. "breaks" gets its groups from
# the cut points, and "missing" always makes exactly two.
GS_GROUP_COUNT_METHODS <- c("quantile", "equal")

# The two levels of a missing-vs-observed variable, in that order: "Observed"
# is FALSE and comes first, so it is the reference level and the left-hand
# panel. They end up in strip text, axis text and file names, so they are
# words rather than FALSE and TRUE.
GS_OBSERVED_LABEL <- "Observed"
GS_MISSING_LABEL <- "Missing"

# The suffix each method gives a derived column when the user has not named it.
# "period" is not here: its suffix names the resolution rather than the method,
# so that a date read by year and the same date read by month do not both come
# out as `admit_period`. See GS_TIME_SUFFIX.
GS_CUT_SUFFIX <- c(quantile = "_cat", equal = "_cat", breaks = "_cat",
                   missing = "_missing")

# --- time resolutions --------------------------------------------------------

# The two families are different questions, not two spellings of one. A
# calendar period is a point on the calendar: every row in "2021-03" is in the
# same month of the same year, and the values run along real time, which is
# what a trend is drawn against. A position in the cycle throws the year away
# and pools every March together, which is what a seasonal pattern is.
#
# Shaped as a list so that selectInput() renders the two families as
# <optgroup>s, the way GS_PALETTES above does.
GS_TIME_UNITS <- list(
  "Calendar period" = list(
    "Year"    = "year",
    "Quarter" = "quarter",
    "Month"   = "month",
    "Week"    = "week",
    "Day"     = "day",
    "Hour"    = "hour",
    "Minute"  = "minute"
  ),
  "Position in the cycle" = list(
    "Month of the year (Jan-Dec)" = "month_of_year",
    "Season"                      = "season",
    "Quarter of the year (Q1-Q4)" = "quarter_of_year",
    "Day of the week (Mon-Sun)"   = "day_of_week",
    "Hour of the day (00-23)"     = "hour_of_day"
  )
)

# The same two families flat, because the validator and the code generator
# branch on them and neither wants to walk the nested list.
GS_PERIOD_UNITS <- c("year", "quarter", "month", "week", "day", "hour",
                     "minute")
GS_CYCLE_UNITS <- c("month_of_year", "season", "quarter_of_year",
                    "day_of_week", "hour_of_day")
GS_TIME_UNIT_VALUES <- c(GS_PERIOD_UNITS, GS_CYCLE_UNITS)

# The resolutions that read a clock rather than a calendar. A plain Date has no
# clock -- data.table's hour() answers 0 for every row rather than failing --
# so these are refused for one, with a reason, instead of silently making a
# column with a single value in it.
GS_TIME_OF_DAY_UNITS <- c("hour", "minute", "hour_of_day")

# The four seasons, in the order they follow the start month the user picks.
# Spring is first because the start month names the start of spring: setting it
# to March gives the northern hemisphere, September the southern.
GS_SEASONS <- c("Spring", "Summer", "Fall", "Winter")
GS_SEASON_START <- 3L

# The suffix each resolution gives a derived column. Two resolutions can share
# one -- a month of the year and a month of the calendar are both "_month" --
# because gs_cut_name() already resolves a collision by counting, and
# `admit_month` and `admit_month2` read better than the alternative of spelling
# `admit_month_of_year` out in full.
GS_TIME_SUFFIX <- c(
  year = "_year", quarter = "_quarter", month = "_month", week = "_week",
  day = "_day", hour = "_hour", minute = "_minute",
  month_of_year = "_month", season = "_season", quarter_of_year = "_quarter",
  day_of_week = "_weekday", hour_of_day = "_hour"
)

# The `date_breaks` string each tick spacing becomes. Quarters are "3 months"
# because ggplot2 has no quarter: "1 quarter" aborts the figure with
# "'from' must be a finite number", which says nothing about what was wrong.
GS_X_TIME_BREAKS <- c(
  minute = "min", hour = "hour", day = "day", week = "week",
  month = "month", quarter = "3 months", year = "year"
)

# The tick spacings offered for the X axis, label to value. The same seven
# words as the calendar periods above, because a tick every month and a
# variable read by month are the same idea applied to the axis rather than to
# the data.
GS_X_TIME_TICKS <- c(Minute = "minute", Hour = "hour", Day = "day",
                     Week = "week", Month = "month", Quarter = "quarter",
                     Year = "year")

# The tick spacings a plain date axis can take. An hour and a minute are not
# among them: scale_x_date() refuses those breaks outright ("invalid
# specification of 'breaks'"), and a column of dates has no hours to tick at
# anyway.
GS_X_TIME_DATE_UNITS <- c("day", "week", "month", "quarter", "year")

# What each resolution is called when it names an axis. Used only when the
# user has left the X-axis label empty; see gs_code_labs().
GS_TIME_UNIT_LABEL <- c(
  year = "Year", quarter = "Quarter", month = "Month", week = "Week",
  day = "Day", hour = "Hour", minute = "Minute"
)

# The tick label formats offered for a date axis. The name of each is the text
# the ticks will actually read, so the control shows its own effect. Blank is
# ggplot2's own choice, which is a good one until the figure has to match a
# particular house style.
#
# %b and %a are the month and day names of the R session's locale, so a figure
# built under a Japanese locale reads differently from one built under an
# English one. That is a property of strftime, not something to paper over: the
# generated code says exactly what was asked for.
GS_TIME_LABELS <- c(
  "Automatic"  = "",
  "2024"       = "%Y",
  "2024-03"    = "%Y-%m",
  "Mar 2024"   = "%b %Y",
  "Mar"        = "%b",
  "2024-03-15" = "%Y-%m-%d",
  "Mar 15"     = "%b %d",
  "15:04"      = "%H:%M"
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
    # A survey weight: a numeric column, never negative. Blank is the
    # unweighted figure the package has always drawn.
    weight      = "",
    # The rest of the survey design, read only when a weight is set: the
    # sampling strata and the clusters (primary sampling units). Blank is a
    # design with neither.
    design_strata  = "",
    design_cluster = "",
    facet       = "",             # the facet_wrap layer: one figure, many panels
    strat_vars  = character(),    # the outer layers: one figure each
    strat_mode  = "independent",  # "independent" | "crossed"
    cuts        = list(),         # categorized continuous variables
    min_n       = 10L,
    show_n      = TRUE,
    jitter      = FALSE,
    err_type    = "se",           # Dot + Error: see GS_ERR_TYPES
    err_level   = 0.95,           # the coverage of an interval, not of an SE
    # Which of a binary outcome's two values counts as the outcome having
    # happened. Read from the data by the server, as group_continuous is,
    # because the answer is a property of the column rather than of a control.
    err_event   = "",
    # Dot + Error: join the points with a line, left to right. What turns a
    # set of independent estimates into a trend over time.
    dot_line    = FALSE,

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
    # The axis ranges, as they were typed. NA at either end is "as far as the
    # data goes", which is what ggplot2 does when it is left to decide.
    xlim_min    = NA_real_,
    xlim_max    = NA_real_,
    ylim_min    = NA_real_,
    ylim_max    = NA_real_,
    # The Line plot's date axis. `x_time_unit` is how far apart the ticks are
    # and `x_time_labels` is a strftime format for what they read; both are
    # blank for the axis ggplot2 would draw on its own.
    x_time_unit   = "",
    x_time_every  = 1,
    x_time_labels = "",
    # Whether the X column holds a date or a date-time, which decides between
    # scale_x_date() and scale_x_datetime(). Set by the server from the
    # classified data, the way group_continuous is; "" in a spec built by hand,
    # which is what keeps the scale out of a figure whose X is a number.
    x_time_class  = "",
    # Tick label angles, in degrees counter-clockwise. 0 is ggplot2's own.
    tick_angle_x = 0,
    tick_angle_y = 0,
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
    dpi         = 300
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

#' One tick angle, or 0 when it is not one of the angles offered
#' @keywords internal
#' @noRd
gs_tick_angle <- function(x) {
  a <- gs_num(x, 0)
  if (is.na(a) || !as.character(a) %in% GS_TICK_ANGLES) 0 else a
}

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

#' Is the figure weighted?
#' @keywords internal
#' @noRd
gs_weighted <- function(spec) nzchar(spec$weight %||% "")

#' Every variable a row has to have a value for to be drawn at all
#'
#' The layer variables, and the survey weight: a row with no weight counts for
#' an unknown amount, which is not the same as counting for nothing, so it is
#' excluded -- and counted -- in the same place and the same way as a row that
#' does not say which panel it belongs to.
#' @keywords internal
#' @noRd
gs_exclude_vars <- function(spec) {
  unique(c(gs_layer_vars(spec), gs_design_vars(spec)))
}

#' The variables that describe how the survey was sampled
#'
#' The weight, the sampling strata and the clusters. A row missing any of them
#' cannot be placed in the design, and is excluded -- and counted -- before the
#' design is built.
#' @keywords internal
#' @noRd
gs_design_vars <- function(spec) {
  if (!gs_weighted(spec)) return(character())
  v <- c(spec$weight, spec$design_strata %||% "", spec$design_cluster %||% "")
  unique(v[nzchar(v)])
}

#' Does the figure need a survey design object, not only a weight column?
#'
#' A Dot + Error bar and a Kaplan-Meier curve are estimated by the survey
#' package; every other figure reads the weight through ggplot2's aesthetic
#' and has no use for the strata or the clusters.
#' @keywords internal
#' @noRd
gs_uses_design <- function(spec) {
  gs_weighted(spec) && spec$plot_type %in% c(GS_DOT, GS_KM)
}

#' Does the figure estimate a variance from the design?
#'
#' A Dot + Error bar always does; a survival curve only when its band is drawn.
#' It is the variance, not the point estimate, that a stratum with a single
#' cluster makes impossible.
#' @keywords internal
#' @noRd
gs_needs_variance <- function(spec) {
  gs_uses_design(spec) &&
    (identical(spec$plot_type, GS_DOT) || isTRUE(spec$km_ci))
}

#' The columns the survey design object is built from
#'
#' The design variables, plus the ones the estimates read out of the design:
#' the Y variable, or a survival curve's time and event. `.svy_row` is the
#' position of each row in the design, which is how a figure's rows find
#' themselves in it.
#' @keywords internal
#' @noRd
gs_design_cols <- function(spec) {
  v <- c(gs_design_vars(spec), spec$y, spec$time, spec$event, ".svy_row")
  unique(v[nzchar(v)])
}

#' The columns a figure is evaluated against
#'
#' What the spec reads, plus the row position a design-based figure uses to
#' find its rows in the design.
#' @keywords internal
#' @noRd
gs_data_cols <- function(spec) {
  c(gs_spec_cols(spec), if (gs_uses_design(spec)) ".svy_row")
}

#' The variables the figure itself reads
#'
#' The layer variables are what a figure is split by; these are what is drawn
#' inside it. Which of them are in use depends on the plot type, so the spec
#' is expected to have been through `gs_normalize_spec()` first -- it is what
#' clears the selections a plot type does not read.
#' @keywords internal
#' @noRd
gs_plotted_vars <- function(spec) {
  v <- c(spec$y, spec$x, spec$group, spec$time, spec$event, spec$id)
  unique(v[nzchar(v)])
}

#' Cuts that split a variable by whether that same variable has a value
#'
#' Stratifying by the missingness of `bmi` puts every row with no `bmi` into
#' one figure. That is the useful thing to do -- until `bmi` is also what the
#' figure draws, and the Missing figure is drawn from rows that are, by
#' construction, all `NA`. It is a real figure with a real N and nothing in
#' it, which reads as a bug in the app rather than as the arrangement the user
#' asked for, so the app says so instead.
#'
#' @return The offending cuts, as a list; empty when there are none.
#' @keywords internal
#' @noRd
gs_self_missing_cuts <- function(spec) {
  layers <- gs_layer_vars(spec)
  plotted <- gs_plotted_vars(spec)
  if (!length(layers) || !length(plotted)) return(list())
  cuts <- gs_as_cuts(spec$cuts)
  if (!length(cuts)) return(list())
  keep <- vapply(cuts, function(cut) {
    identical(cut$method, "missing") &&
      cut$new %in% layers && cut$var %in% plotted
  }, logical(1L))
  cuts[keep]
}

#' How many layers a spec describes, the described variable included
#'
#' The described variable, plus one for each variable that panels or splits
#' it. What a given count looks like is the user's arrangement, not this
#' number: two layers is a `facet_wrap()` when the second variable is the
#' facet, and one figure per level when it is a stratifying variable instead.
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
  # The date axis belongs to the Line plot, and to a Line plot whose X really
  # is a date. Cleared rather than reported: a setting left in a box the user
  # cannot currently see is not a mistake worth refusing to draw a figure over,
  # and the control says beside itself what it applies to.
  if (!identical(spec$plot_type, GS_LINE) || !nzchar(spec$x_time_class)) {
    spec$x_time_unit <- ""
    spec$x_time_labels <- ""
  }
  if (!spec$x_time_unit %in% c("", GS_PERIOD_UNITS)) spec$x_time_unit <- ""
  # A date carries no time of day, and scale_x_date() does not merely ignore an
  # hourly tick spacing -- it refuses the figure. Dropped here, where the
  # column's type is known, rather than left to fail at draw time.
  if (identical(spec$x_time_class, "date") &&
      !spec$x_time_unit %in% c("", GS_X_TIME_DATE_UNITS)) {
    spec$x_time_unit <- ""
  }
  # The two settings are independent: a format with no spacing relabels the
  # ticks ggplot2 chose, which is often all that is wanted. A count without a
  # spacing to count is not, so it goes back to one.
  every <- as.integer(gs_num(spec$x_time_every, 1))
  spec$x_time_every <- if (!nzchar(spec$x_time_unit) || is.na(every) ||
                           every < 1L) 1L else every
  # The error bar belongs to one plot type, so a setting left behind by
  # another can never reach the generated code -- and, with it, the helper
  # function that setting would have needed defining.
  if (!identical(spec$plot_type, GS_DOT)) {
    spec$err_type <- "se"
    spec$dot_line <- FALSE
  }
  if (!spec$err_type %in% GS_ERR_PROP_TYPES) spec$err_event <- ""
  if (!spec$err_type %in% GS_ERR_TYPES) spec$err_type <- "se"
  level <- gs_num(spec$err_level, 0.95)
  spec$err_level <- if (is.na(level) || level <= 0 || level >= 1) 0.95 else level
  if (!spec$plot_type %in% GS_SMOOTH_TYPES) spec$smooth <- FALSE
  # loess reads a weight as the precision of an observation, not as the number
  # of people it stands for, so the band it draws around a weighted fit is not
  # a design-based interval. The UI hides the box while a weight is set.
  if (gs_weighted(spec)) spec$smooth_se <- FALSE
  # Strata and clusters describe how a weighted sample was drawn, and mean
  # nothing without the weight; the UI hides them while there is none.
  if (!gs_weighted(spec)) {
    spec$design_strata <- ""
    spec$design_cluster <- ""
  }
  spec$tick_angle_x <- gs_tick_angle(spec$tick_angle_x)
  spec$tick_angle_y <- gs_tick_angle(spec$tick_angle_y)
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
#'
#' `x_time_class` is a property of the data rather than of a control, so it
#' arrives as an argument. It has to be here rather than assigned afterwards:
#' `gs_spec()` normalizes what it builds, and normalizing is what clears a
#' date-axis setting that does not apply -- which it would do to every one of
#' them if it were told only later that the X column is a date.
#' @keywords internal
#' @noRd
gs_spec_from_input <- function(input, x_time_class = "", err_event = "") {
  none <- function(x) if (is.null(x) || identical(x, "") || identical(x, GS_NONE)) "" else x
  gs_spec(
    plot_type  = input$plot_type %||% "Boxplot",
    x          = none(input$xvar),
    y          = none(input$yvar),
    time       = none(input$timevar),
    event      = none(input$eventvar),
    id         = none(input$idvar),
    group      = none(input$group),
    weight     = none(input$weight),
    design_strata  = none(input$design_strata),
    design_cluster = none(input$design_cluster),
    facet      = none(input$facet),
    strat_vars = input$strat_vars %||% character(),
    strat_mode = input$strat_mode %||% "independent",
    # Clearing the numeric box yields NA, which must not poison the size rules.
    min_n      = as.integer(gs_num(input$min_n, 10)),
    # NULL before the checkbox has registered; its UI default is TRUE.
    show_n     = !identical(input$show_n, FALSE),
    jitter     = isTRUE(input$jitter),
    err_type   = input$err_type %||% "se",
    err_level  = gs_num(input$err_level, 0.95),
    err_event  = err_event,
    dot_line   = isTRUE(input$dot_line),
    line_points = isTRUE(input$line_points),
    smooth     = isTRUE(input$smooth),
    smooth_se  = isTRUE(input$smooth_se),
    smooth_span = gs_num(input$smooth_span, 0.75),
    x_time_unit   = none(input$x_time_unit),
    x_time_every  = gs_num(input$x_time_every, 1),
    x_time_labels = input$x_time_labels %||% "",
    x_time_class  = x_time_class,
    tick_angle_x  = gs_num(input$tick_angle_x, 0),
    tick_angle_y  = gs_num(input$tick_angle_y, 0),
    binwidth   = gs_num(input$binwidth),
    alpha      = gs_num(input$alpha, 0.6),
    bw_adjust  = gs_num(input$bw_adjust, 1),
    km_ci      = isTRUE(input$km_ci),
    # NULL before the checkboxes have registered; both default to TRUE.
    km_censor  = !identical(input$km_censor, FALSE),
    km_ylim    = !identical(input$km_ylim, FALSE),
    km_risk    = isTRUE(input$km_risk),
    # An empty box is NA, which is the "let ggplot2 decide" the default is.
    xlim_min   = gs_num(input$xlim_min),
    xlim_max   = gs_num(input$xlim_max),
    ylim_min   = gs_num(input$ylim_min),
    ylim_max   = gs_num(input$ylim_max),
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
  cols <- c(spec$x, spec$y, spec$group, spec$facet, spec$id, spec$weight %||% "",
            spec$design_strata %||% "", spec$design_cluster %||% "")
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
  # categorization removes its column. Saying so beats letting the plot code
  # fail on a missing object, and nothing else is worth reporting until the
  # selection is valid again.
  if (!is.null(info) && nrow(info)) {
    gone <- setdiff(gs_spec_cols(spec), gs_vars_of(info, "all"))
    if (length(gone)) {
      return(sprintf("'%s' is no longer a column in the data.", gone))
    }
  }

  # Checked for every plot type, and before the Kaplan-Meier branch returns.
  # An inverted range is silently destructive rather than loud: coord_cartesian
  # draws an empty panel, and on a survival curve the number-at-risk times are
  # the ones inside the range, so a backwards range leaves the table with no
  # columns at all.
  problems <- c(problems, gs_validate_limits(spec))
  # And so is the weight, which every type but one reads.
  problems <- c(problems, gs_validate_weight(spec, info))

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
    # A proportion interval is drawn from an outcome that happened or did not,
    # so it asks the opposite of the usual question: the Y variable has to hold
    # two values, which is exactly what disqualifies it as a measurement.
    if (identical(type, GS_DOT) && spec$err_type %in% GS_ERR_PROP_TYPES) {
      if (nzchar(spec$y) && !spec$y %in% gs_vars_of(info, "binary")) {
        problems <- c(problems, sprintf(
          paste0("A proportion interval counts how often something happened, ",
                 "so '%s' has to have exactly two values -- 0 and 1, TRUE and ",
                 "FALSE, or a factor with two levels."),
          spec$y))
      }
    } else if (identical(type, GS_DOT) && nzchar(spec$y) &&
               spec$y %in% gs_vars_of(info, "binary")) {
      # A 0/1 column is not a measurement, so the general message is right
      # about why it is refused and useless about what to do instead. There is
      # something to do instead, and it is two controls away.
      problems <- c(problems, sprintf(
        paste0("'%s' is 0 and 1 rather than a measurement, so a mean and a ",
               "standard error do not describe it. Set the bar to one of the ",
               "proportion intervals."), spec$y))
    } else if (nzchar(needs_cont) && !needs_cont %in% continuous) {
      problems <- c(problems, sprintf(
        "'%s' does not look continuous; %s expects a continuous variable there.",
        needs_cont, type))
    }
    # Only Scatter is held to a continuous X. A line plot's X is time, which
    # is as often a date -- which is not classified as continuous -- as it is
    # a measurement.
    if (type == "Scatter" && nzchar(spec$x) && !spec$x %in% continuous) {
      problems <- c(problems,
                    sprintf("Scatter expects a continuous X-variable; '%s' is not.", spec$x))
    }
  }

  problems
}

#' Axis ranges that a figure cannot be drawn in
#'
#' Both ends of a range are optional -- one alone means "from here" or "up to
#' here" -- so the check only applies when both are given. Equal ends are
#' rejected alongside reversed ones: a range of zero width is as undrawable as
#' a backwards one.
#' @keywords internal
#' @noRd
gs_validate_limits <- function(spec) {
  one <- function(lo, hi, axis) {
    lo <- gs_num(lo)
    hi <- gs_num(hi)
    if (is.na(lo) || is.na(hi) || lo < hi) return(character())
    sprintf(paste("The %s-axis range runs backwards: %s is not below %s.",
                  "Swap them, or clear one to leave that end free."),
            axis, gs_n(lo), gs_n(hi))
  }
  c(one(spec$xlim_min, spec$xlim_max, "X"),
    one(spec$ylim_min, spec$ylim_max, "Y"))
}

#' A survey weight that cannot be used
#'
#' Refused rather than dropped: a figure drawn unweighted under a weight the
#' user chose would be a different description from the one on the screen,
#' with nothing to say so.
#' @keywords internal
#' @noRd
gs_validate_weight <- function(spec, info = NULL) {
  if (!gs_weighted(spec)) return(character())
  if (spec$plot_type %in% GS_UNWEIGHTED_TYPES) {
    return(sprintf(paste0(
      "A %s draws one dot per row, so it cannot carry a survey weight. ",
      "Clear the weight, or use a Histogram or a Boxplot instead."),
      spec$plot_type))
  }
  if (!is.null(info) && nrow(info) && !spec$weight %in% gs_vars_of(info, "weight")) {
    return(sprintf(paste0(
      "The survey weight must be a number that is never negative; '%s' is ",
      "not."), spec$weight))
  }
  if (nzchar(spec$design_strata %||% "") &&
      identical(spec$design_strata, spec$design_cluster)) {
    return(sprintf(paste0(
      "'%s' cannot be both the sampling strata and the clusters. A stratum is ",
      "a group sampled separately; a cluster is one unit sampled within it."),
      spec$design_strata))
  }
  character()
}

#' A survey design whose variance cannot be estimated
#'
#' A stratum that holds a single cluster has no second cluster to measure the
#' spread between clusters against, and survey stops with "Stratum has only
#' one PSU" when it is asked for a variance. Reported here, by name, with what
#' to do about it, rather than as that error from inside the figure. Only a
#' figure that estimates a variance is refused: a survival curve without its
#' band needs none.
#'
#' Checked against the data, not the classification, because it is a question
#' about how the rows fall into strata and clusters.
#' @param dt The data, before any row is set aside.
#' @keywords internal
#' @noRd
gs_validate_design <- function(dt, spec) {
  if (!gs_needs_variance(spec)) return(character())
  strata <- spec$design_strata %||% ""
  cluster <- spec$design_cluster %||% ""
  if (!nzchar(strata) && !nzchar(cluster)) return(character())
  if (!all(gs_design_vars(spec) %in% names(dt))) return(character())

  d <- dt[gs_complete_layers(dt, gs_design_vars(spec))]
  psu <- if (nzchar(cluster)) as.character(d[[cluster]]) else as.character(seq_len(nrow(d)))
  s <- if (nzchar(strata)) as.character(d[[strata]]) else rep("", nrow(d))
  # Counted within each stratum, as nest = TRUE reads a cluster ID.
  n_psu <- vapply(split(psu, s), function(x) length(unique(x)), integer(1L))
  lonely <- names(n_psu)[n_psu < 2L]
  if (!length(lonely)) return(character())

  if (!nzchar(strata)) {
    return(sprintf(paste0(
      "'%s' has only one cluster, so a variance between clusters cannot be ",
      "estimated."), cluster))
  }
  sprintf(paste0(
    "Sampling stratum %s of '%s' %s only one %s, so the variance within it ",
    "cannot be estimated. Merge %s with a neighbouring stratum before calling ",
    "ggstratify()."),
    paste(sprintf("'%s'", lonely), collapse = ", "), strata,
    if (length(lonely) == 1L) "has" else "have",
    if (nzchar(cluster)) "cluster" else "row",
    if (length(lonely) == 1L) "it" else "each")
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
