# The UI is built once and never regenerated; the server only pushes new
# *choices* into the selectors when a data set is loaded. Plot-type-dependent
# controls are hidden with conditionalPanel() so that switching plot type is a
# pure client-side operation with no server round-trip.

#' Build the ggstratify user interface
#'
#' There is no control for loading data. The data set is the argument the
#' launcher was called with, already read and already typed by the user; see
#' the *Before you start* section of [ggstratify()].
#' @keywords internal
#' @noRd
gs_ui <- function() {
  bslib::page_sidebar(
    title = "ggstratify",
    # font_scale multiplies Bootstrap's base font size, so every control,
    # label and table in the app grows together. The exported PNGs are drawn
    # by ggplot2 and are unaffected by it.
    theme = bslib::bs_theme(version = 5, preset = "flatly", font_scale = 1.35),
    sidebar = bslib::sidebar(
      width = 400,
      open = "desktop",

      bslib::accordion(
        multiple = TRUE,
        open = c("Describe", "Layers"),

        bslib::accordion_panel(
          "Describe",
          shiny::selectInput("plot_type", "Type of graph", GS_PLOT_TYPES,
                             selected = "Boxplot"),
          shiny::conditionalPanel(
            condition = gs_js_not_km(),
            shiny::selectInput("yvar", "Y-variable (continuous)", choices = NULL)
          ),
          shiny::conditionalPanel(
            condition = sprintf("!%s.includes(input.plot_type) && %s",
                                gs_js_array(GS_XONLY_TYPES), gs_js_not_km()),
            shiny::selectInput("xvar", "X-variable", choices = NULL)
          ),
          shiny::conditionalPanel(
            condition = gs_js_km(),
            shiny::selectInput("timevar", "Time variable", choices = NULL),
            shiny::selectInput("eventvar", "Event (1 = event, 0 = censored)",
                               choices = NULL)
          ),
          shiny::conditionalPanel(
            condition = gs_js_type(GS_LINE),
            shiny::selectInput("idvar", "One line per (e.g. patient ID)",
                               choices = NULL),
            shiny::helpText(
              "Leave this empty when each x value already has one value to",
              "draw -- a mean per visit, say. Choose the subject identifier",
              "to draw one line per subject instead."
            )
          ),
          shiny::selectInput("group", "Group (colour by this variable)",
                             choices = NULL)
        ),

        bslib::accordion_panel(
          "Categorize",
          shiny::helpText(
            "Turn a continuous variable into groups. The new variable can be",
            "used as a layer like any other categorical variable"
          ),
          shiny::selectInput("cut_var", "Continuous variable", choices = NULL),
          shiny::selectInput("cut_method", "How", choices = GS_CUT_METHODS),
          shiny::conditionalPanel(
            condition = "input.cut_method != 'breaks'",
            shiny::numericInput("cut_n", "Number of groups", value = 4,
                                min = 2, max = 20, step = 1)
          ),
          shiny::conditionalPanel(
            condition = "input.cut_method == 'breaks'",
            shiny::textInput("cut_points", "Cut points, comma separated",
                             placeholder = "50, 65, 80"),
            shiny::helpText(
              "Cut points are boundaries: 65 gives (-Inf, 65] and (65, Inf]."
            )
          ),
          shiny::textInput("cut_name", "New variable name"),
          shiny::actionButton("add_cut", "Add categorized variable",
                              class = "btn-secondary w-100",
                              icon = shiny::icon("plus")),
          shiny::uiOutput("cuts_list")
        ),

        bslib::accordion_panel(
          "Layers",
          shiny::helpText(
            "Choose how to split your figure using additional variables.",
            "Leave both boxes empty: create one figure with all data.",
            "Panels: show multiple small plots within one figure, one for",
            "each level of the variable.",
            "Separate figures: create a separate figure for each level of",
            "the variable.",
            "A variable can be used in either Panels or Separate figures,",
            "but not both."
          ),
          shiny::selectInput("facet", "Panels side by side in one figure",
                             choices = GS_NONE),
          shiny::checkboxGroupInput("strat_vars",
                                    "Separate figures (one file per level)",
                                    choices = character()),
          shiny::conditionalPanel(
            condition = "input.strat_vars && input.strat_vars.length > 1",
            shiny::radioButtons("strat_mode", NULL, choices = GS_STRAT_MODES,
                                selected = "independent")
          ),
          shiny::uiOutput("layer_summary"),
          shiny::numericInput("min_n", "Minimum rows per separate figure",
                              value = 10, min = 0, step = 1),
          shiny::helpText(
            "No separate figure is created for a group with fewer rows than",
            "this number. Set to 0 to include every non-empty group."
          ),
          shiny::checkboxInput("show_n",
                               "Show N in the title and on the panel strips",
                               TRUE),
          shiny::helpText(
            "Every figure says how many observations it was drawn from, and",
            "so does every panel inside it.",
            "A stratum with N = 0 (an unused factor level) is always listed",
            "on the Strata tab and never drawn.",
            "Rows missing a layer variable are excluded, and counted"
          )
        ),

        bslib::accordion_panel(
          "Plot options",
          shiny::conditionalPanel(
            condition = "['Boxplot','Violin','Dot + Error'].includes(input.plot_type)",
            shiny::checkboxInput("jitter", "Add jittered points", FALSE)
          ),
          shiny::conditionalPanel(
            condition = "['Histogram','Dotplot'].includes(input.plot_type)",
            shiny::numericInput("binwidth", "Binwidth (blank = automatic)",
                                value = NA, min = 0)
          ),
          shiny::conditionalPanel(
            condition = "input.plot_type == 'Density'",
            shiny::sliderInput("bw_adjust", "Bandwidth adjustment",
                               min = 0.1, max = 3, value = 1, step = 0.1)
          ),
          shiny::conditionalPanel(
            condition = gs_js_type(GS_LINE),
            shiny::checkboxInput("line_points", "Mark the observations", FALSE)
          ),
          shiny::conditionalPanel(
            condition = sprintf("%s.includes(input.plot_type)",
                                gs_js_array(GS_SMOOTH_TYPES)),
            shiny::checkboxInput("smooth", "Add a LOWESS smoother", FALSE),
            shiny::conditionalPanel(
              condition = "input.smooth == true",
              shiny::sliderInput("smooth_span", "Smoother span (larger = smoother)",
                                 min = 0.1, max = 1, value = 0.75, step = 0.05),
              shiny::checkboxInput("smooth_se", "Show the smoother's 95% band",
                                   FALSE),
              shiny::helpText(
                "Drawn with a grouping variable, the smoother is fitted",
                "separately within each group."
              )
            )
          ),
          shiny::conditionalPanel(
            condition = gs_js_km(),
            shiny::checkboxInput("km_ci", "Show 95% confidence band", FALSE),
            shiny::checkboxInput("km_censor", "Mark censored observations", TRUE),
            shiny::checkboxInput("km_ylim", "Y axis from 0 to 1", TRUE),
            shiny::checkboxInput("km_risk", "Add the number-at-risk table",
                                 FALSE),
            shiny::conditionalPanel(
              condition = "input.km_risk == true",
              shiny::helpText(
                "Drawn under the curve, one row per group, counted at the",
                "times the x axis is labelled at. The tail of a survival",
                "curve is read differently when 12 people are left than when",
                "300 are."
              )
            )
          ),
          shiny::sliderInput("alpha", "Alpha (opacity)",
                             min = 0.1, max = 1, value = 0.6, step = 0.05),
          shiny::checkboxInput(
            "sample_big",
            "Speed up previews of very large datasets",
            TRUE
          ),
          shiny::helpText(
            "For the on-screen preview only, randomly use a subset of rows",
            "when there are more than 1,000,000 rows for Scatter and Line",
            "plots, or more than 10,000,000 rows for other plot types.",
            "This does not remove groups or change which figures are made.",
            "Exports and generated code always use all rows. Kaplan-Meier",
            "curves are never sampled."
          )
        ),

        bslib::accordion_panel(
          "Appearance",
          shiny::selectInput("theme", "Theme", choices = GS_THEMES,
                             selected = "theme_bw()"),
          shiny::selectInput("palette", "Colour palette", choices = GS_PALETTES),
          shiny::helpText(
            "A palette colours the groups, so it only changes the figure once",
            "a Group (or colour) variable has been chosen under Describe."
          ),
          shiny::textInput("title", "Title (blank = stratum name)"),
          shiny::textInput("lab_x", "X-axis label"),
          shiny::textInput("lab_y", "Y-axis label"),
          shiny::textInput("lab_legend", "Legend label"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput("xlim_min", "X axis from",
                                                 value = NA)),
            shiny::column(6, shiny::numericInput("xlim_max", "X axis to",
                                                 value = NA))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput("ylim_min", "Y axis from",
                                                 value = NA)),
            shiny::column(6, shiny::numericInput("ylim_max", "Y axis to",
                                                 value = NA))
          ),
          shiny::helpText(
            "Blank is automatic, and either end can be left blank on its own.",
            "The figure is zoomed into the range rather than filtered to it,",
            "so a boxplot's median and a density's shape stay the ones the",
            "whole data gives. A range typed here replaces a Kaplan-Meier",
            "curve's 'Y axis from 0 to 1'."
          )
        ),

        bslib::accordion_panel(
          "Export",
          shiny::textInput("data_name", "Data object name (used in the code)",
                           value = "mydata"),
          shiny::textInput("outdir", "Output folder", value = "figures"),
          shiny::helpText(
            "Relative to the working directory. The folder is created if it",
            "does not exist yet."
          ),
          shiny::textInput("prefix", "File-name prefix (blank = plot type)"),
          shiny::selectInput("format", "File format", GS_FORMATS,
                             selected = "png"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput("width", "Width (in)",
                                                 value = 7, min = 1, step = 0.5)),
            shiny::column(6, shiny::numericInput("height", "Height (in)",
                                                 value = 5, min = 1, step = 0.5))
          ),
          shiny::conditionalPanel(
            condition = "input.format != 'svg'",
            shiny::numericInput("dpi", "Resolution (dpi)", value = 300,
                                min = 72, step = 10)
          ),
          shiny::actionButton("export", "Export all figures",
                              class = "btn-primary w-100",
                              icon = shiny::icon("download"))
        )
      )
    ),

    bslib::navset_card_tab(
      id = "main_tabs",

      bslib::nav_panel(
        "Plot",
        shiny::div(
          class = "d-flex flex-wrap align-items-end gap-4 mb-2",
          # The variables decide how many figures there are, so they decide
          # what the preview can show. The switch is offered only when both
          # answers are possible: with no separate figures there is only one
          # figure to show, and with the panels already spent on a facet
          # variable there is nowhere to put the others. The server keeps the
          # selection in step, so nothing here needs explaining on screen.
          shiny::conditionalPanel(
            condition = gs_js_has_strata(),
            shiny::radioButtons(
              "preview_mode", "Show",
              choices = c("All figures at once" = "facet",
                          "One figure at a time" = "single"),
              selected = "facet", inline = TRUE
            )
          ),
          # Which figure to show is a question only when there is more than
          # one, and only when they are shown one at a time -- which a facet
          # variable makes true whatever the switch above says.
          shiny::conditionalPanel(
            condition = sprintf("%s && (input.preview_mode == 'single' || %s)",
                                gs_js_strat_vars(), gs_js_has_facet()),
            shiny::selectInput("preview_stratum", "Which figure",
                               choices = NULL, width = "360px")
          )
        ),
        shiny::uiOutput("preview_note"),
        # Tall enough that the figure keeps roughly the 7:5 shape the export
        # defaults to once GS_PREVIEW_RES has been applied to it.
        shiny::plotOutput("plot", height = "660px")
      ),

      bslib::nav_panel(
        "Data",
        shiny::helpText("First 200 rows."),
        shiny::div(class = "gs-scroll", shiny::tableOutput("data_table"))
      ),

      bslib::nav_panel(
        "Strata",
        shiny::helpText(
          "Every figure the current layers produce, with the number of",
          "observations behind it. A stratum with no file name gets no",
          "figure: either it has N = 0, or it is below the minimum N. The",
          "line above says how many of each."
        ),
        shiny::uiOutput("strata_summary"),
        shiny::div(class = "gs-scroll", shiny::tableOutput("strata_table"))
      ),

      bslib::nav_panel(
        "R-code",
        shiny::helpText(
          "The ggplot2 code for the figure on the Plot tab, and nothing else.",
          "Writing the files is what the Export button is for; what is worth",
          "taking away from here is the figure itself, to paste into your own",
          "script and edit."
        ),
        shiny::div(
          class = "mb-2",
          shiny::actionButton("copy_code", "Copy to clipboard",
                              icon = shiny::icon("clipboard"))
        ),
        shiny::verbatimTextOutput("code")
      )
    ),

    shiny::tags$head(shiny::tags$style(shiny::HTML(GS_CSS))),
    shiny::tags$script(shiny::HTML(GS_COPY_JS))
  )
}

#' Render a string as a quoted JavaScript string literal
#'
#' Every conditionalPanel() condition below is built by pasting one of the
#' package's own constants into a fragment of JavaScript, so nothing hostile
#' can reach it. The escaping is here so that adding a plot type or a theme
#' whose name happens to contain an apostrophe -- `Tufte's boxplot` -- breaks
#' nothing more than the label it is written in.
#' @keywords internal
#' @noRd
gs_js_str <- function(x) {
  # fixed = TRUE on both: the strings being escaped are labels, not patterns.
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("'", "\\'", x, fixed = TRUE)
  paste0("'", x, "'")
}

#' Render a character vector as a JavaScript array literal
#' @keywords internal
#' @noRd
gs_js_array <- function(x) {
  paste0("[", paste0(gs_js_str(x), collapse = ","), "]")
}

#' conditionalPanel() condition for a plot-type-only control
#' @keywords internal
#' @noRd
gs_js_type <- function(type) paste0("input.plot_type == ", gs_js_str(type))

#' conditionalPanel() conditions for the Kaplan-Meier-only controls
#' @keywords internal
#' @noRd
gs_js_km <- function() gs_js_type(GS_KM)

#' @rdname gs_js_km
#' @keywords internal
#' @noRd
gs_js_not_km <- function() paste0("input.plot_type != ", gs_js_str(GS_KM))

#' conditionalPanel() condition: at least one variable splits the figures
#' @keywords internal
#' @noRd
gs_js_strat_vars <- function() {
  "input.strat_vars && input.strat_vars.length > 0"
}

#' conditionalPanel() condition: a variable is panelling the figure
#' @keywords internal
#' @noRd
gs_js_has_facet <- function() paste0("input.facet != ", gs_js_str(GS_NONE))

#' conditionalPanel() condition: the preview could show either mode
#'
#' Both modes are possible only when the figures are split *and* the panels
#' are still free, because showing every figure at once borrows the same
#' `facet_wrap()` a facet variable would use.
#' @keywords internal
#' @noRd
gs_js_has_strata <- function() {
  sprintf("%s && !(%s)", gs_js_strat_vars(), gs_js_has_facet())
}

# bs_theme(font_scale=) raises Bootstrap's own base size, and everything
# Bootstrap sizes in `em` follows it. What is left here is the text that would
# not: rules written in px by Shiny or by the slider widget, and the handful
# Bootstrap deliberately shrinks. Sizes are given in `em` rather than `rem` on
# purpose -- `rem` is measured from the unscaled root font, so a rule in `rem`
# would pull this text back down instead of leaving it scaled.
GS_CSS <- "
#code {
  background: #f6f8fa;
  color: #24292f;
  border: 1px solid #d7dde3;
  border-radius: 6px;
  padding: 14px 16px;
  font-size: 0.95em;
  line-height: 1.55;
  max-height: 70vh;
  overflow: auto;
}
.accordion-button { font-weight: 600; }
.gs-problem { color: #b02a37; font-weight: 500; }
.gs-scroll { max-height: 65vh; overflow: auto; }
.gs-scroll table { font-size: 0.95em; }
.gs-skip { color: #b02a37; }

/* Notes, hints and the alerts the sidebar and the Strata tab use, all of
   which Bootstrap sets to 0.875em. */
.small, small, .form-text, .help-block, .alert { font-size: 0.95em; }

/* Labels, so a control is as easy to read as its value. */
.form-label, .control-label, .shiny-input-container > label {
  font-weight: 600;
}

/* ionRangeSlider ships its own px sizes, which no theme scale reaches. */
.irs-min, .irs-max, .irs-single, .irs-from, .irs-to { font-size: 0.8em; }
.irs-grid-text { font-size: 0.7em; }

/* The message shiny::validate() paints over the plot area. */
.shiny-output-error-validation { font-size: 1.05em; color: #b02a37; }
"

# Copying happens in the browser so that it also works over a remote session.
#
# The Clipboard API is only available in a secure context, which localhost is
# but a plain http:// address on another machine is not, so the button has to
# survive not having it: execCommand('copy') over a temporary textarea is the
# older path every browser still honours. Either way the button says what
# happened -- a copy button that silently does nothing leaves the user
# pasting whatever was on the clipboard before.
GS_COPY_JS <- "
$(document).on('click', '#copy_code', function() {
  var el = document.getElementById('code');
  if (!el) { return; }
  var btn = this;
  // Read once and remember: a second click while the button is still
  // saying 'Copied' would otherwise make that the label it restores.
  if (!btn.dataset.gsLabel) { btn.dataset.gsLabel = btn.innerHTML; }
  var label = btn.dataset.gsLabel;
  var say = function(msg) {
    btn.innerHTML = msg;
    setTimeout(function() { btn.innerHTML = label; }, 1500);
  };
  var fallback = function() {
    var ta = document.createElement('textarea');
    ta.value = el.innerText;
    ta.setAttribute('readonly', '');
    ta.style.position = 'fixed';
    ta.style.left = '-9999px';
    document.body.appendChild(ta);
    ta.select();
    var ok = false;
    try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
    document.body.removeChild(ta);
    say(ok ? 'Copied' : 'Press Ctrl+C to copy');
  };
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(el.innerText).then(
      function() { say('Copied'); }, fallback);
  } else {
    fallback();
  }
});
"
