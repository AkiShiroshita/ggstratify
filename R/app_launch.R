# The one exported entry point. Argument checking is done with checkmate so
# that a mistake is reported here, in one clear message, rather than surfacing
# as a broken reactive several layers into the Shiny app.

#' Describe a data set, one layer at a time
#'
#' Launches a point-and-click Shiny interface for descriptive analysis: pick
#' the variable you want to describe, then add the layers you want to see it
#' within. The figure, the number of observations behind it and the R code
#' that reproduces it all appear together.
#'
#' @section The layers:
#'
#' A description has a variable being described and, around it, the variables
#' you want to see it within. `ggstratify` calls those the layers, and asks
#' you to place each one:
#'
#' \describe{
#'   \item{One layer}{The variable on its own. A plain `ggplot2` figure.}
#'   \item{Two layers}{The second variable becomes the panels of a
#'     `facet_wrap()`, so the whole comparison is one figure.}
#'   \item{Three or more}{Two layers is what a single figure holds. Beyond
#'     that, you choose: one variable stays as the `facet_wrap()` panels, and
#'     the rest become separate figures -- one file each. With columns A, B, C
#'     and D you might describe D, panel it by C, and get one figure per
#'     level of A and of B.}
#' }
#'
#' The variables that make separate figures can be treated separately, which
#' is the default -- ticking `sex` and `treatment` gives the figures `sex_M`,
#' `sex_F`, `treatment_A`, ... -- or crossed, giving one figure per observed
#' combination. Separately is usually the right question for descriptive
#' work: crossing two five-level variables gives twenty-five mostly empty
#' figures.
#'
#' @section What it tells you:
#'
#' Every level is reported with the number of observations it contains, and
#' that number is written into the figure title and onto each panel strip
#' inside it: a strip reads `site: Site A (N = 303)` rather than `Site A`, so
#' that a level which does not explain itself -- the bare range a categorized
#' variable produces -- still says which variable it is a level of. A level
#' with no observations at all (an unused factor level) is listed with
#' `N = 0` and produces no figure. Rows with a missing value in any layer
#' variable are excluded -- a row that does not say which panel it belongs to
#' cannot be drawn in one -- and the number excluded is reported on the Strata
#' tab and by the generated code.
#'
#' @section Three things descriptive work keeps needing:
#'
#' Under **Categorize**, a continuous variable becomes a categorical one --
#' quantile groups, equal-width bins or your own cut points -- and can then be
#' used as a layer like any other categorical variable.
#'
#' Under **Type of graph**, *Kaplan-Meier curve* draws survival curves from a
#' time variable and an event indicator, fitted with [survival::survfit()]
#' within each group, panel and figure, optionally with a confidence band,
#' censoring marks and a number-at-risk table under the curve. *Line* draws
#' change over time: put time on the X axis and the measurement on the Y axis,
#' and name the subject identifier under **One line per** to get one line per
#' subject.
#'
#' A *Line* or *Scatter* figure can carry a LOWESS smoother -- `stats::loess()`
#' through [ggplot2::geom_smooth()] -- with its span under your control and,
#' when a grouping variable is set, one fit per group.
#'
#' @section Before you start:
#'
#' Convert each column to the type you mean it to have -- `numeric` for
#' measurements, `factor` for groups, with the levels in the order you want
#' them read -- before passing the data. `ggstratify` describes what it is
#' given; it does not guess what you meant. A grouping variable stored as
#' `1, 2, 3` will be described as a number.
#'
#' This is why the data must be an object you already have in your session.
#' There is no file to choose from inside the application, and no file path to
#' hand it: a data set that arrives by being read from disk arrives with types
#' that a reader guessed, and it is then described on those guessed types
#' without anyone having looked at them. Read the file yourself, run
#' [str()] or [summary()] over the result, fix the types that are wrong, and
#' pass that object:
#'
#' ```
#' cohort <- read.csv("cohort.csv")
#' cohort$treatment <- factor(cohort$treatment,
#'                            levels = c("Control", "Low dose", "High dose"))
#' str(cohort)
#' ggstratify(cohort)
#' ```
#'
#' The figure types, themes and palettes otherwise match those offered by the
#' \pkg{ggplotgui} package. All data handling uses \pkg{data.table}. Figures
#' are written as PNG (through \pkg{ragg}) or as SVG, and the "R-code" tab
#' shows the self-contained \pkg{ggplot2} code for the figure on screen --
#' just the figure, since writing the files is what the export button is for.
#' The preview, the export button and that code are all produced by the same
#' code generator, so the code you are shown is the code that made the
#' figure.
#'
#' @param dataset The data to describe, and the one thing the function needs:
#'   a data frame -- including a `tibble` or a `data.table` -- or a matrix,
#'   already read into your session and already of the types you mean. A file
#'   path is not accepted; see *Before you start*. Whatever it is given becomes
#'   a plain `data.table`, so a grouped `tibble` is described as its rows, not
#'   as its groups. The object is copied, never modified in place, and its name
#'   is what the generated code refers to it by.
#' @param launch.browser Passed to [shiny::runApp()]. `TRUE` opens the system
#'   browser.
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @return Invisibly `NULL`, after the app is closed. Called for its side
#'   effect of running a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   ggstratify(iris)
#'   ggstratify(epi_cohort)
#' }
#' @export
ggstratify <- function(dataset, launch.browser = TRUE, ...) {
  # Read before `dataset` is forced, so that the name in the generated code is
  # the expression the user wrote rather than the value it evaluated to.
  data_name <- gs_data_name(substitute(dataset))
  checkmate::assert_flag(launch.browser)
  checkmate::assert(
    checkmate::check_data_frame(dataset, min.cols = 1L),
    checkmate::check_matrix(dataset, min.cols = 1L),
    .var.name = "dataset",
    combine = "or"
  )
  # Fail here rather than inside the app, where the message is easy to miss.
  dataset <- gs_prepare_data(dataset)

  app <- shiny::shinyApp(
    ui = gs_ui(),
    server = gs_server(dataset, data_name),
    # ragg is a markedly faster raster device than the default on Windows.
    onStart = function() {
      old <- options(shiny.useragg = TRUE)
      shiny::onStop(function() options(old))
    }
  )

  shiny::runApp(app, launch.browser = launch.browser, ...)
  invisible(NULL)
}

#' The name the generated code should call the data by
#'
#' `ggstratify(cohort)` should print code that starts `d <- cohort`, not
#' `d <- mydata`. Anything that is not a plain variable name -- a subset, a
#' pipeline, a literal -- would not be a name the reader could paste, so those
#' fall back to the placeholder the Export panel starts with, which the user
#' can overwrite.
#'
#' @param expr The unevaluated `dataset` argument, from `substitute()`.
#' @return A single syntactic name.
#' @keywords internal
#' @noRd
gs_data_name <- function(expr) {
  if (!is.name(expr)) return("mydata")
  nm <- as.character(expr)
  if (!nzchar(nm) || !identical(make.names(nm), nm)) return("mydata")
  nm
}
