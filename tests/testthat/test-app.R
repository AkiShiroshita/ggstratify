scatter_spec <- gs_spec(plot_type = "Scatter", x = "x", y = "y")
box_spec <- gs_spec(plot_type = "Boxplot", y = "y")

test_that("gs_downsample leaves small data alone", {
  d <- data.table::data.table(x = rnorm(100), y = rnorm(100))
  expect_identical(gs_downsample(d, scatter_spec, TRUE), d)
  expect_identical(gs_downsample(d, box_spec, TRUE), d)
})

# The real caps are in the millions of rows, so the sampling *rules* are tested
# against a small threshold passed in, and the caps themselves are tested as
# the numbers they are. Building a table of the real cap's size to watch it be
# halved would cost more memory than the check machine is promised.
CAP <- 500L

test_that("gs_downsample is a no-op when the user switches it off", {
  d <- data.table::data.table(x = rnorm(CAP + 100L))
  expect_identical(gs_downsample(d, scatter_spec, FALSE, threshold = CAP), d)
})

test_that("the caps are large enough that ordinary data is never sampled", {
  # Sampling is the one place where the preview stops matching the data, so it
  # has to be reached by data that will not draw, not by data that is merely
  # large. A cohort of a million rows draws its own boxplot.
  expect_gte(gs_sample_threshold(scatter_spec), 1e6)
  expect_gte(gs_sample_threshold(box_spec), 1e7)
})

test_that("scatter is capped harder than the other plot types", {
  expect_lt(gs_sample_threshold(scatter_spec), gs_sample_threshold(box_spec))
  # A line plot costs the same as a scatter plot of the same data.
  expect_equal(gs_sample_threshold(gs_spec(plot_type = GS_LINE)),
               gs_sample_threshold(scatter_spec))

  d <- data.table::data.table(x = rnorm(CAP * 2L))
  expect_equal(nrow(gs_downsample(d, scatter_spec, TRUE, threshold = CAP)), CAP)
  # The same data is under the real cap for either type, so it is left whole.
  expect_equal(nrow(gs_downsample(d, scatter_spec, TRUE)), nrow(d))
  expect_equal(nrow(gs_downsample(d, box_spec, TRUE)), nrow(d))
})

test_that("a sampled line plot keeps whole trajectories", {
  # Half a trajectory is a line nobody has, so subjects are sampled, not rows.
  per_id <- 4L
  n_id <- CAP                          # 4x the cap, in rows
  d <- data.table::data.table(
    id = rep(sprintf("P%05d", seq_len(n_id)), each = per_id),
    visit = rep(seq_len(per_id), times = n_id),
    crp = rnorm(n_id * per_id)
  )
  spec <- gs_spec(plot_type = GS_LINE, x = "visit", y = "crp", id = "id")
  out <- gs_downsample(d, spec, TRUE, threshold = CAP)

  expect_lte(nrow(out), CAP)
  expect_gt(nrow(out), 0L)
  # Every ID that survived kept all of its visits.
  expect_true(all(out[, .N, by = id]$N == per_id))
})

test_that("group-wise sampling keeps small strata intact", {
  n <- CAP + 1000L
  d <- data.table::data.table(
    y = rnorm(n),
    .strat_label = c(rep("g: huge", n - 20L), rep("g: tiny", 20L))
  )
  out <- gs_downsample(d, box_spec, TRUE, by = ".strat_label", threshold = CAP)

  expect_lte(nrow(out), CAP)
  # Every stratum survives, and the tiny one is not thinned at all.
  expect_setequal(unique(out$.strat_label), c("g: huge", "g: tiny"))
  expect_equal(nrow(out[.strat_label == "g: tiny"]), 20L)
})

# --- server wiring ------------------------------------------------------------

gs_test_inputs <- function(outdir) {
  list(
    plot_type = "Boxplot", yvar = "Sepal.Length", xvar = GS_NONE,
    group = GS_NONE, strat_vars = "Species", min_n = 10,
    strat_mode = "independent",
    jitter = TRUE, binwidth = NA, alpha = 0.6, bw_adjust = 1,
    idvar = GS_NONE, line_points = FALSE, smooth = FALSE, smooth_se = FALSE,
    smooth_span = 0.75,
    facet = GS_NONE, theme = "theme_bw()",
    palette = "", title = "", lab_x = "", lab_y = "", lab_legend = "",
    xlim_min = NA, xlim_max = NA, ylim_min = NA, ylim_max = NA,
    data_name = "iris", outdir = outdir, prefix = "", width = 7, height = 5,
    dpi = 300, format = "png", preview_mode = "facet", preview_stratum = "1",
    sample_big = TRUE, show_n = TRUE,
    timevar = "", eventvar = "", km_ci = FALSE, km_censor = TRUE,
    km_ylim = TRUE, km_risk = FALSE, cut_var = "Sepal.Length",
    cut_method = "quantile", cut_n = 4, cut_points = "", cut_name = "",
    cut_unit = "month", cut_season_start = "3",
    x_time_unit = "", x_time_every = 1, x_time_labels = "",
    err_type = "se", err_level = 0.95, err_event = "",
    tick_angle_x = "0", tick_angle_y = "0"
  )
}

test_that("the server renders, lists strata and generates code", {
  outdir <- file.path(tempdir(), "gs-test-server")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$elapse(500)

    expect_equal(nrow(dat()), 150L)
    expect_equal(gs_vars_of(info(), "stratify"), "Species")
    expect_length(problems(), 0L)

    expect_equal(nrow(strata()), 3L)
    expect_true(all(strata()$keep))
    expect_true(facet_preview())
    expect_no_error(output$plot)

    # The code is one figure's, and says which one.
    expect_match(output$code, "p <- ggplot(", fixed = TRUE)
    expect_false(grepl("ggsave", output$code, fixed = TRUE))
    expect_match(output$code, "# Figure: Species: setosa", fixed = TRUE)

    # Single-stratum preview.
    session$setInputs(preview_mode = "single", preview_stratum = "2")
    session$elapse(500)
    expect_false(facet_preview())
    expect_no_error(output$plot)
  })
})

test_that("the export button writes one PNG per kept stratum", {
  outdir <- file.path(tempdir(), "gs-test-export")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$elapse(500)
    session$setInputs(export = 1)

    expect_equal(
      sort(list.files(outdir, pattern = "\\.png$")),
      c("boxplot_Species_setosa.png", "boxplot_Species_versicolor.png",
        "boxplot_Species_virginica.png")
    )
  })
  unlink(outdir, recursive = TRUE)
})

test_that("the code on the R-code tab runs, and draws the figure shown", {
  # The preview, the export and this code are all the same generated text
  # evaluated against the same rows, so what has to hold is that the text
  # runs on its own -- in a session that has never heard of ggstratify.
  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(preview_mode = "single", preview_stratum = "2")
    session$elapse(500)

    env <- new.env(parent = globalenv())
    assign("iris", iris, envir = env)
    p <- eval(parse(text = output$code), envir = env)

    expect_s3_class(p, "ggplot")
    # The second stratum, as the selector says, and only it.
    expect_equal(nrow(p$data), 50L)
    expect_equal(as.character(unique(p$data$Species)), "versicolor")
  })
})

test_that("the export button writes what the Strata tab says it will", {
  outdir <- file.path(tempdir(), "gs-svg")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(format = "svg")
    session$elapse(500)
    session$setInputs(export = 1)

    expect_equal(sort(list.files(outdir)),
                 sort(paste0("boxplot_", strata()[keep == TRUE, file], ".svg")))
  })
  expect_length(list.files(outdir, pattern = "\\.svg$"), 3L)
  unlink(outdir, recursive = TRUE)
})

test_that("a facet layer disables the all-figures preview rather than double-faceting", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = "sex", facet = "severity")
    session$elapse(500)

    expect_false(facet_preview())
    expect_no_error(output$plot)
    expect_match(output$code, "facet_wrap(~ .facet_label)", fixed = TRUE)
  })
})

test_that("one variable cannot be two layers at once", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", strat_vars = c("sex", "severity"),
                      facet = "severity")
    session$elapse(500)

    # Faceting by it wins; splitting the figures by it as well would leave one
    # panel per figure.
    expect_equal(spec_r()$strat_vars, "sex")
    expect_equal(spec_r()$facet, "severity")
    expect_no_error(output$plot)
  })
})

test_that("switching plot type keeps the app in a drawable state", {
  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$elapse(500)

    # The survival curve is exercised separately: iris has nothing to follow up.
    for (type in setdiff(GS_PLOT_TYPES, GS_KM)) {
      # Scatter and Line put a variable on each axis.
      session$setInputs(plot_type = type,
                        xvar = if (type %in% GS_XY_TYPES) "Sepal.Width" else "Species")
      session$elapse(500)
      expect_length(problems(), 0L)
      expect_no_error(output$plot)
    }
  })
})

# --- the preview mode ---------------------------------------------------------

test_that("the chosen variables, not the switch, decide what the preview shows", {
  # The switch itself is only offered when both answers are possible, and the
  # server keeps its value in step with updateRadioButtons(). What is asserted
  # here is the part that survives a stale switch: the mock session never
  # feeds an update back into input$, so preview_mode stays at "facet"
  # throughout and every mode below is the one the variables imply.
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = character(), facet = GS_NONE)
    session$elapse(500)

    # Nothing splits the figures, so there is only one figure to show.
    expect_false(preview_choice())
    expect_false(facet_preview())
    expect_no_error(output$plot)

    # Ticking a variable makes several: show them all at once.
    session$setInputs(strat_vars = "sex")
    session$elapse(500)
    expect_true(preview_choice())
    expect_true(facet_preview())

    # Panelling by a variable spends the panels the all-figures preview needs,
    # so it goes back to one at a time -- without the user touching anything.
    session$setInputs(facet = "severity")
    session$elapse(500)
    expect_false(preview_choice())
    expect_false(facet_preview())
    expect_no_error(output$plot)

    session$setInputs(facet = GS_NONE)
    session$elapse(500)
    expect_true(preview_choice())
    expect_true(facet_preview())
  })
})

test_that("changing how a figure looks does not change which figure is shown", {
  # update*Input() moves a control back to its first choice, and the reactives
  # behind the two preview controls are invalidated by every input in the
  # sidebar -- not only by the ones that decide what there is to preview. So
  # what has to hold is that nothing is pushed back to the browser at all when
  # the appearance changes; the mock session cannot show it, because it never
  # feeds an update into input$, so the two update calls are recorded instead.
  pushed <- character()
  record <- function(session, inputId, ...) {
    pushed <<- c(pushed, inputId)
    invisible(NULL)
  }
  local_mocked_bindings(updateSelectInput = record,
                        updateRadioButtons = record, .package = "shiny")

  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(preview_mode = "single", preview_stratum = "3")
    session$elapse(500)
    expect_equal(current_stratum()$label, "Species: virginica")
    # The figures themselves were pushed once, when the data arrived.
    expect_true("preview_stratum" %in% pushed)

    for (change in list(list(alpha = 0.3), list(theme = "theme_minimal()"),
                        list(palette = "Set1"), list(jitter = FALSE),
                        list(ylim_min = 4.5))) {
      pushed <<- character()
      do.call(session$setInputs, change)
      session$elapse(500)
      expect_equal(pushed, character(), info = names(change))
    }
    expect_equal(current_stratum()$label, "Species: virginica")

    # A variable that does decide what there is to preview still pushes the
    # new list, or the box would go on offering figures that are not drawn.
    pushed <<- character()
    session$setInputs(strat_vars = character())
    session$elapse(500)
    expect_true("preview_stratum" %in% pushed)
  })
})

test_that("a typed axis range reaches the figure and the code beside it", {
  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(ylim_min = 4.5, ylim_max = 7)
    session$elapse(500)

    expect_equal(spec_r()$ylim_min, 4.5)
    expect_match(output$code, "coord_cartesian(ylim = c(4.5, 7))", fixed = TRUE)
    expect_no_error(output$plot)

    # Clearing a box is NA, which is the automatic range it started with.
    session$setInputs(ylim_min = NA, ylim_max = NA)
    session$elapse(500)
    expect_false(grepl("coord_cartesian", output$code, fixed = TRUE))
  })
})

# --- line plots ---------------------------------------------------------------

test_that("the server draws a line plot and writes the ID into the code", {
  # Repeated measurements per patient, which is what a line plot describes.
  # The patients with no CRP are dropped here: epi_cohort leaves that column
  # incomplete on purpose, and this test is about lines, not about gaps.
  visits <- data.table::rbindlist(lapply(1:6, function(v) {
    d <- data.table::as.data.table(epi_cohort)[!is.na(crp),
                                               .(id, sex, treatment, crp)]
    d[, visit := v]
    d[, crp := crp * (1 - 0.15 * v)]
    d
  }))

  shiny::testServer(gs_server(visits), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = GS_LINE, xvar = "visit", yvar = "crp",
                      idvar = "id", group = "treatment", strat_vars = "sex",
                      line_points = TRUE, smooth = TRUE, smooth_span = 0.8,
                      data_name = "visits")
    session$elapse(500)

    expect_length(problems(), 0L)
    expect_equal(spec_r()$id, "id")
    expect_no_error(output$plot)

    expect_match(output$code, "geom_line(aes(group = id), alpha = 0.6)",
                 fixed = TRUE)
    expect_match(output$code, "geom_point(size = 1, alpha = 0.6)", fixed = TRUE)
    expect_match(output$code, 'geom_smooth(method = "loess", formula = y ~ x, span = 0.8',
                 fixed = TRUE)

    # The ID is a column the figure reads, so it survives the narrowing.
    expect_true("id" %in% gs_spec_cols(spec_r()))

    # Switching away takes the line-only options with it.
    session$setInputs(plot_type = "Boxplot", xvar = "treatment")
    session$elapse(500)
    expect_equal(spec_r()$id, "")
    expect_false(grepl("geom_smooth", output$code, fixed = TRUE))
  })
})

# --- Kaplan-Meier -------------------------------------------------------------

test_that("the server draws and exports stratified survival curves", {
  outdir <- file.path(tempdir(), "gs-test-km")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(plot_type = GS_KM, timevar = "fu_days",
                      eventvar = "death", group = "treatment",
                      strat_vars = "sex", data_name = "epi_cohort")
    session$elapse(500)

    expect_length(problems(), 0L)
    # The time and event variables replace the x/y selection entirely.
    expect_equal(spec_r()$x, "")
    expect_equal(spec_r()$time, "fu_days")
    expect_no_error(output$plot)

    expect_match(output$code, "km_data <- function", fixed = TRUE)
    expect_match(output$code, 'km_data(d, "fu_days", "death", by = c("treatment"))',
                 fixed = TRUE)

    # The risk table is drawn on request, and reaches the code with the curve.
    session$setInputs(km_risk = TRUE)
    session$elapse(500)
    expect_match(output$code, "km_risk <- function", fixed = TRUE)
    expect_no_error(output$plot)
    session$setInputs(km_risk = FALSE)
    session$elapse(500)

    session$setInputs(preview_mode = "single", preview_stratum = "1")
    session$elapse(500)
    expect_no_error(output$plot)

    session$setInputs(export = 1)
  })

  expect_equal(sort(list.files(outdir, pattern = "\\.png$")),
               c("kaplan_meier_curve_sex_Female.png",
                 "kaplan_meier_curve_sex_Male.png"))
  unlink(outdir, recursive = TRUE)
})

test_that("the app says what a survival curve is missing rather than erroring", {
  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = GS_KM, timevar = "Sepal.Length",
                      eventvar = "Species")
    session$elapse(500)

    expect_gt(length(problems()), 0L)
    expect_match(output$code, "^# Cannot generate code yet")
  })
})

# --- categorizing -------------------------------------------------------------

test_that("a categorized variable joins the selectors and can be stratified on", {
  outdir <- file.path(tempdir(), "gs-test-cut")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(yvar = "los_days", xvar = "sex", strat_vars = character(),
                      data_name = "epi_cohort", cut_var = "age",
                      cut_method = "breaks", cut_points = "65",
                      cut_name = "age65")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    expect_length(cuts_used(), 1L)
    expect_true("age65" %in% names(dat()))
    expect_true("age65" %in% gs_vars_of(info(), "stratify"))
    # The original data is untouched; only the app's copy grew a column.
    expect_false("age65" %in% names(epi_cohort))

    # The rule reaches the spec, and therefore the generated script.
    expect_equal(spec_r()$cuts[[1]]$new, "age65")
    expect_match(output$code, "dt[, age65 := cut(age, breaks = c(-Inf, 65, Inf))]",
                 fixed = TRUE)

    session$setInputs(strat_vars = "age65")
    session$elapse(500)
    expect_equal(nrow(strata()), 2L)
    expect_no_error(output$plot)
    session$setInputs(export = 1)

    # Removing the rule takes the column and the stratification with it.
    session$setInputs(cut_remove = "1", remove_cut = 1)
    session$elapse(500)
    expect_length(cuts_used(), 0L)
    expect_false("age65" %in% names(dat()))
    expect_length(spec_r()$strat_vars, 0L)
    expect_false(grepl("age65", output$code, fixed = TRUE))
  })

  expect_length(list.files(outdir, pattern = "\\.png$"), 2L)
  unlink(outdir, recursive = TRUE)
})

test_that("an impossible categorization is reported and not added", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(cut_var = "age", cut_method = "breaks",
                      cut_points = "sixty five", cut_name = "age65")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    expect_length(cuts_used(), 0L)
    expect_false("age65" %in% names(dat()))
  })
})

test_that("adding a categorized variable leaves the other selections alone", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", xvar = "treatment", group = "sex",
                      strat_vars = "site", cut_var = "age",
                      cut_method = "quantile", cut_n = 3, cut_name = "age_cat")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    spec <- spec_r()
    expect_equal(spec$y, "los_days")
    expect_equal(spec$x, "treatment")
    expect_equal(spec$group, "sex")
    expect_equal(spec$strat_vars, "site")
  })
})

test_that("the app reports a problem instead of erroring on an unusable spec", {
  shiny::testServer(gs_server(iris), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Boxplot", yvar = "Species")
    session$elapse(500)

    expect_gt(length(problems()), 0L)
    expect_match(output$code, "^# Cannot generate code yet")
  })
})

# --- empty strata -------------------------------------------------------------

test_that("an n = 0 stratum is listed but never exported", {
  outdir <- file.path(tempdir(), "gs-empty")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = "site", min_n = 10)
    session$elapse(500)

    st <- strata()
    expect_equal(nrow(st), 4L)                       # Site D is included
    expect_equal(st[level == "Site D", n], 0L)
    expect_false(st[level == "Site D", keep])
    expect_match(st[level == "Site D", status], "n = 0")
    expect_no_error(output$strata_table)
    expect_no_error(output$plot)

    session$setInputs(export = 1)
  })

  files <- list.files(outdir, pattern = "\\.png$")
  expect_length(files, 3L)                           # A, B, C -- not D
  expect_false(any(grepl("Site_D", files)))
  unlink(outdir, recursive = TRUE)
})

test_that("the strata summary counts empty and undersized strata", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", strat_vars = c("site", "severity"),
                      min_n = 30)
    session$elapse(500)

    st <- strata()
    expect_equal(sum(st$n == 0L), 1L)                # Site D
    expect_gt(sum(!st$keep & st$n > 0L), 0L)         # Severe
    expect_true(all(st[keep == TRUE, n] >= 30L))
    expect_no_error(output$strata_summary)
  })
})

test_that("the stratum size reaches the figure title and the facet strips", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = "sex", min_n = 10, preview_mode = "single",
                      preview_stratum = "1", show_n = TRUE)
    session$elapse(500)
    expect_match(output$code, 'labs(title = "sex_Male (N = ', fixed = TRUE)

    session$setInputs(show_n = FALSE)
    session$elapse(500)
    expect_match(output$code, 'labs(title = "sex_Male")', fixed = TRUE)
  })

  # Facet strips carry the size too.
  dt <- gs_prepare_data(epi_cohort)
  long <- gs_long_strata(dt, "sex", c("treatment", "los_days"), 10L)
  expect_true(all(grepl("\\(N = \\d+\\)$", levels(long$.strat_label))))

  # ...and follow the same switch the title does.
  plain <- gs_long_strata(dt, "sex", c("treatment", "los_days"), 10L,
                          label_n = FALSE)
  expect_false(any(grepl("N = ", levels(plain$.strat_label), fixed = TRUE)))
})

# --- the layers ---------------------------------------------------------------

test_that("one layer is a plain figure and no code loops over anything", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", xvar = GS_NONE,
                      strat_vars = character(), facet = GS_NONE,
                      data_name = "epi_cohort")
    session$elapse(500)

    expect_equal(gs_n_layers(spec_r()), 1L)
    expect_equal(nrow(strata()), 0L)
    expect_match(output$code, "d <- epi_cohort", fixed = TRUE)
    expect_false(grepl("facet_", output$code, fixed = TRUE))
    expect_no_error(output$plot)
  })
})

test_that("two layers are one figure of panels, three are separate figures", {
  outdir <- file.path(tempdir(), "gs-test-layers")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = character(), facet = "severity",
                      data_name = "epi_cohort")
    session$elapse(500)

    expect_equal(gs_n_layers(spec_r()), 2L)
    # The panels carry their sizes, so they are drawn from a labelled copy.
    expect_match(output$code, "facet_wrap(~ .facet_label)", fixed = TRUE)
    expect_match(output$code, 'add_facet_n(d, "severity")', fixed = TRUE)

    # A third layer no longer fits in one figure, so it makes several.
    session$setInputs(strat_vars = "sex")
    session$elapse(500)
    expect_equal(gs_n_layers(spec_r()), 3L)
    expect_match(output$code, 'd <- dt[as.character(sex) == "Male"]',
                 fixed = TRUE)
    session$setInputs(export = 1)
  })

  expect_equal(sort(list.files(outdir, pattern = "\\.png$")),
               c("boxplot_sex_Female.png", "boxplot_sex_Male.png"))
  unlink(outdir, recursive = TRUE)
})

test_that("crossed layers give one figure per observed combination", {
  by_button <- file.path(tempdir(), "gs-crossed-button")
  unlink(by_button, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(by_button))
    session$setInputs(yvar = "los_days", xvar = "treatment",
                      strat_vars = c("sex", "site"), strat_mode = "crossed",
                      min_n = 0, data_name = "epi_cohort")
    session$elapse(500)

    st <- strata()
    # Site D recruited nobody, so six of the eight combinations occur.
    expect_equal(nrow(st), 6L)
    expect_equal(attr(st, "n_possible"), 8L)
    expect_true(all(grepl("^sex: (Male|Female) \\| site: Site [ABC]$", st$label)))
    expect_true(all(grepl("^sex_(Male|Female)__site_Site_[ABC]$", st$file)))
    expect_equal(sum(st$n), nrow(epi_cohort))
    expect_no_error(output$plot)

    session$setInputs(export = 1)

    # The code for one of those figures subsets on both variables at once.
    expect_match(output$code, "as.character(sex) == ", fixed = TRUE)
    expect_match(output$code, "as.character(site) == ", fixed = TRUE)
    env <- new.env(parent = globalenv())
    assign("epi_cohort", epi_cohort, envir = env)
    expect_s3_class(eval(parse(text = output$code), envir = env), "ggplot")
  })

  expect_length(list.files(by_button, pattern = "\\.png$"), 6L)
  unlink(by_button, recursive = TRUE)
})

test_that("rows missing a layer variable are excluded and counted", {
  cohort <- data.table::as.data.table(epi_cohort)
  cohort[1:40, sex := NA]
  cohort[1:15, severity := NA]
  n_kept <- nrow(cohort) - 40L

  shiny::testServer(gs_server(cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "los_days", strat_vars = "sex",
                      facet = "severity", data_name = "cohort")
    session$elapse(500)

    # The 15 rows missing severity are among the 40 missing sex, so 40 go.
    expect_equal(nrow(layer_data()), n_kept)
    expect_equal(missing_report()$n_missing, c(40L, 15L))
    expect_equal(sum(strata()$n), n_kept)
    expect_false(any(is.na(strata()$level)))
    expect_no_error(output$strata_summary)

    # And the generated script does the same thing, out loud.
    expect_match(output$code, 'layer_vars <- c("sex", "severity")', fixed = TRUE)
    expect_match(output$code, "Excluded ", fixed = TRUE)
  })
})

test_that("the UI builds and carries every control the server reads", {
  # as.character() renders the tags without pulling in htmltools by name.
  ui <- as.character(gs_ui())
  for (id in c("plot_type", "yvar", "xvar", "group", "timevar", "eventvar",
               "km_ci", "km_risk", "idvar", "line_points", "smooth",
               "smooth_span", "smooth_se", "cut_var", "cut_method", "add_cut",
               "facet", "strat_vars", "strat_mode", "min_n", "show_n",
               "export", "format", "outdir", "preview_mode", "strata_table",
               "layer_summary", "xlim_min", "xlim_max", "ylim_min",
               "ylim_max", "cut_unit", "cut_season_start", "x_time_unit",
               "x_time_every", "x_time_labels", "err_type", "err_level",
               "tick_angle_x", "tick_angle_y", "err_event", "weight",
               "design_strata", "design_cluster")) {
    expect_true(grepl(id, ui, fixed = TRUE), info = id)
  }
})

test_that("every selector opens on nothing chosen", {
  # A pre-filled box is a claim the app is not in a position to make, and a
  # figure drawn before anything was asked for looks like one that was.
  choices <- gs_selector_choices(gs_classify_vars(gs_prepare_data(epi_cohort)))

  expect_setequal(names(choices),
                  c("yvar", "xvar", "group", "idvar", "weight",
                    "design_strata", "design_cluster", "facet",
                    "timevar", "eventvar", "cut_var"))
  for (id in names(choices)) {
    expect_equal(choices[[id]][1], GS_NONE, info = id)
  }
  # Each list still offers the variables it is meant to.
  expect_true("los_days" %in% choices$yvar)
  expect_true("fu_days" %in% choices$timevar)
  expect_true("death" %in% choices$eventvar)
  expect_false("death" %in% choices$timevar[-1] && "sex" %in% choices$eventvar)
  expect_true("site" %in% choices$facet)
  expect_false("id" %in% choices$facet)     # too many levels to split on
})

test_that("nothing is drawn until something has been chosen", {
  shiny::testServer(gs_server(epi_cohort), {
    session$setInputs(plot_type = "Boxplot")
    session$elapse(500)

    expect_equal(spec_r()$y, "")
    expect_equal(spec_r()$facet, "")
    expect_gt(length(problems()), 0L)
    expect_match(output$code, "^# Cannot generate code yet")
  })
})

test_that("gs_device refuses a format it has no device for", {
  expect_identical(gs_device("png"), ragg::agg_png)
  expect_true(is.function(gs_device("svg")))
  expect_error(gs_device("pdf"), "Unknown figure format")
})

test_that("the JavaScript the conditionalPanels are built from is escaped", {
  expect_equal(gs_js_str("Boxplot"), "'Boxplot'")
  expect_equal(gs_js_str("Tufte's boxplot"), "'Tufte\\'s boxplot'")
  expect_equal(gs_js_str("a\\b"), "'a\\\\b'")
  expect_equal(gs_js_array(c("a", "b")), "['a','b']")
  # The real constants must survive the round trip unchanged.
  expect_equal(gs_js_type(GS_KM),
               sprintf("input.plot_type == '%s'", GS_KM))
  expect_equal(gs_js_has_facet(), sprintf("input.facet != '%s'", GS_NONE))
})

test_that("a missingness variable can be derived and stratified on in the app", {
  outdir <- file.path(tempdir(), "gs-test-missing")
  unlink(outdir, recursive = TRUE)

  # severity is a factor, so it is offered only under "missing" -- the pool
  # the method chooses is tested in test-categorize.R; here it has to survive
  # the whole way to a figure on disk.
  gappy <- data.table::as.data.table(epi_cohort)
  gappy[seq_len(120L), severity := NA]

  shiny::testServer(gs_server(gappy), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(yvar = "los_days", xvar = GS_NONE,
                      strat_vars = character(), data_name = "gappy",
                      cut_var = "severity", cut_method = "missing",
                      cut_name = "severity_missing")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    expect_length(cuts_used(), 1L)
    expect_equal(levels(dat()$severity_missing), c("Observed", "Missing"))
    expect_true("severity_missing" %in% gs_vars_of(info(), "stratify"))
    expect_match(output$code,
                 "severity_missing := factor(is.na(severity)", fixed = TRUE)

    session$setInputs(strat_vars = "severity_missing")
    session$elapse(500)

    st <- strata()
    expect_equal(nrow(st), 2L)
    expect_setequal(st$level, c("Observed", "Missing"))
    # Every row is placed: the two groups add up, and nothing is excluded for
    # having no value for the layer variable.
    expect_equal(sum(st$n), nrow(gappy))
    expect_equal(nrow(missing_report()), 0L)

    expect_no_error(output$plot)
    session$setInputs(export = 1)
  })

  expect_equal(sort(list.files(outdir, pattern = "[.]png$")),
               c("boxplot_severity_missing_Missing.png",
                 "boxplot_severity_missing_Observed.png"))
  unlink(outdir, recursive = TRUE)
})

test_that("describing a variable within its own missingness is warned about", {
  gappy <- data.table::as.data.table(epi_cohort)
  gappy[seq_len(120L), bmi := NA_real_]

  # The arrangement under test is the one where the Missing figure has nothing
  # to draw, so rendering its preview makes ggplot2 report the rows it dropped
  # -- the very consequence the app is warning about. Shiny renders outputs
  # inside its own promise domain, out of reach of a calling handler placed in
  # here, so the warning is suppressed around the session as a whole. Errors
  # still surface, and the app's own sentence is asserted below.
  suppressWarnings(shiny::testServer(gs_server(gappy), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(yvar = "bmi", xvar = GS_NONE, strat_vars = character(),
                      cut_var = "bmi", cut_method = "missing",
                      cut_name = "bmi_missing")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    # Describing something else within the split says nothing.
    session$setInputs(yvar = "los_days", strat_vars = "bmi_missing")
    session$elapse(500)
    expect_false(any(grepl("no bmi to draw",
                           as.character(output$layer_summary), fixed = TRUE)))

    # Describing bmi within it does.
    session$setInputs(yvar = "bmi")
    session$elapse(500)
    expect_true(any(grepl("no bmi to draw",
                          as.character(output$layer_summary), fixed = TRUE)))
    # The figure is still drawn: the app reports the arrangement, it does not
    # refuse it.
    expect_no_error(output$plot)
  }))
})

test_that("a variable with nothing missing is refused with a reason", {
  shiny::testServer(gs_server(epi_cohort), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(cut_var = "age", cut_method = "missing",
                      cut_name = "age_missing")
    session$elapse(500)
    session$setInputs(add_cut = 1)
    session$elapse(500)

    expect_length(cuts_used(), 0L)
    expect_false("age_missing" %in% names(dat()))
  })
})

# --- reading a date at a resolution, through the app --------------------------

test_that("a date can be read by month and described within it", {
  # The whole of the feature, end to end: a column of dates that cannot be a
  # layer becomes one, the months come out January to December, and the code
  # on screen is code that runs.
  outdir <- file.path(tempdir(), "gs-test-period")
  unlink(outdir, recursive = TRUE)

  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(outdir))
    session$setInputs(cut_method = "period", cut_var = "admit_date",
                      cut_unit = "month_of_year", cut_name = "admit_month")
    session$elapse(500)

    # Before the rule is added there is nothing to stratify by but the date's
    # own hundreds of values, which is to say nothing.
    expect_false("admit_date" %in% gs_vars_of(info(), "stratify"))

    session$setInputs(add_cut = 1)
    session$elapse(500)

    expect_true("admit_month" %in% names(dat()))
    expect_equal(levels(dat()$admit_month), month.abb)
    expect_true("admit_month" %in% gs_vars_of(info(), "stratify"))

    # Describe length of stay by month, the way the seasonal figure is drawn.
    # los_days rather than crp so that the figure is not also a demonstration
    # of ggplot2 dropping the 122 rows with no CRP.
    session$setInputs(plot_type = "Boxplot", yvar = "los_days",
                      xvar = "admit_month", strat_vars = character(),
                      data_name = "epi_cohort")
    session$elapse(500)

    expect_length(problems(), 0L)
    expect_no_error(output$plot)

    # The printed script says how the column was made, in the idiom it would
    # have been written in by hand.
    expect_match(output$code,
                 "factor(month.abb[month(admit_date)], levels = month.abb)",
                 fixed = TRUE)
  })
})

test_that("a season can be started at a month of the user's choosing", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(cut_method = "period", cut_var = "admit_date",
                      cut_unit = "season", cut_season_start = "9",
                      cut_name = "admit_season", add_cut = 1)
    session$elapse(500)

    col <- dat()$admit_season
    expect_equal(levels(col), GS_SEASONS)
    # September starts spring, which is what choosing the month is for; the
    # order of the four names does not move with it.
    september <- col[data.table::month(dat()$admit_date) == 9L]
    expect_equal(unique(as.character(september)), "Spring")
  })
})

test_that("a time resolution the column cannot answer is refused with a reason", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    # An hour of the day off a plain date would be midnight for every row.
    session$setInputs(cut_method = "period", cut_var = "admit_date",
                      cut_unit = "hour_of_day", cut_name = "admit_hour",
                      add_cut = 1)
    session$elapse(500)
    expect_false("admit_hour" %in% names(dat()))
    expect_length(cuts(), 0L)
  })
})

test_that("the name box follows the resolution but never a name that was typed", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(cut_method = "period", cut_var = "admit_date",
                      cut_unit = "year")
    session$elapse(500)
    # testServer does not feed updateTextInput() back into input$, so the
    # suggestion is read where the server keeps it.
    expect_equal(suggested_name(), "admit_date_year")

    session$setInputs(cut_unit = "season")
    session$elapse(500)
    expect_equal(suggested_name(), "admit_date_season")

    # A name the user typed is left alone, even one that looks like a name the
    # app might have suggested. `birth_year` is a column an analyst has.
    session$setInputs(cut_name = "birth_year", cut_unit = "month_of_year")
    session$elapse(500)
    expect_equal(suggested_name(), "admit_date_season")
  })
})

test_that("a line plot draws a date axis ticked and titled by its unit", {
  # epi_cohort has one row per patient, so this is a line through the cohort
  # rather than one per subject; what is being checked is the axis.
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Line", yvar = "los_days",
                      xvar = "admit_date",
                      strat_vars = character(), x_time_unit = "year",
                      x_time_labels = "%Y", data_name = "epi_cohort")
    session$elapse(500)

    expect_equal(spec_r()$x_time_class, "date")
    expect_length(problems(), 0L)
    expect_match(output$code,
                 'scale_x_date(date_breaks = "1 year", date_labels = "%Y")',
                 fixed = TRUE)
    # The column drawn is a date; the axis is told to say what the ticks mean.
    expect_match(output$code, 'labs(x = "Year")', fixed = TRUE)
    expect_no_error(output$plot)
  })
})

test_that("a numeric axis range typed against a date axis does not break the figure", {
  # The X boxes are numeric and a date cannot be typed into one. Handing the
  # number through would stop ggplot2 outright rather than zoom anything.
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Line", yvar = "los_days",
                      xvar = "admit_date",
                      strat_vars = character(), xlim_min = 10, xlim_max = 100,
                      data_name = "epi_cohort")
    session$elapse(500)

    expect_false(grepl("coord_cartesian(xlim", output$code, fixed = TRUE))
    expect_no_error(output$plot)
  })
})

# --- what the error bar stands for --------------------------------------------

test_that("a Dot + Error bar can be a confidence interval instead of a standard error", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Dot + Error", yvar = "los_days",
                      xvar = "severity", strat_vars = character(),
                      jitter = FALSE, data_name = "epi_cohort")
    session$elapse(500)

    # The default is what it has always been, and needs nothing defined.
    expect_length(problems(), 0L)
    expect_match(output$code, "stat_summary(fun.data = mean_se", fixed = TRUE)
    expect_false(grepl("mean_ci <- function", output$code, fixed = TRUE))

    session$setInputs(err_type = "normal", err_level = 0.99)
    session$elapse(500)

    expect_length(problems(), 0L)
    # The interval, the level it was asked for, and the function that computes
    # it, printed above the figure that uses it.
    expect_match(output$code, "mean_ci <- function(x, conf = 0.95)", fixed = TRUE)
    expect_match(output$code,
                 "stat_summary(fun.data = mean_ci, fun.args = list(conf = 0.99)",
                 fixed = TRUE)
    expect_no_error(output$plot)
  })
})

test_that("a proportion is described by a proportion interval, not by a mean", {
  # `death` is 0/1, so it is not a measurement -- which is exactly what the
  # proportion intervals are for.
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Dot + Error", yvar = "death",
                      xvar = "severity", strat_vars = character(),
                      jitter = FALSE, data_name = "epi_cohort")
    session$elapse(500)

    # A mean and a standard error is refused, and says what to do instead.
    expect_match(problems(), "proportion intervals", all = FALSE)

    for (type in GS_ERR_PROP_TYPES) {
      session$setInputs(err_type = type)
      session$elapse(500)
      expect_length(problems(), 0L)
      expect_no_error(output$plot)
    }

    expect_match(output$code, "prop_ci_wilson <- function", fixed = TRUE)
    # And the figure really is bounded, which is the point of choosing one.
    env <- new.env(parent = asNamespace("ggstratify"))
    assign("epi_cohort", epi_cohort, envir = env)
    p <- eval(parse(text = output$code), envir = env)
    bars <- ggplot2::ggplot_build(p)$data[[1L]]
    expect_true(all(bars$ymin >= 0 & bars$ymax <= 1))
  })
})

test_that("a 0/1 outcome is offered as a Y variable, but only where it can be drawn", {
  # `death` is 0 and 1, so it is not continuous -- which used to keep it out of
  # the Y list altogether, including on the one figure that can describe it.
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Boxplot")
    session$elapse(500)
    expect_false("death" %in%
                   gs_selector_choices(info(), "quantile", "Boxplot")$yvar)

    session$setInputs(plot_type = "Dot + Error")
    session$elapse(500)
    expect_true("death" %in%
                  gs_selector_choices(info(), "quantile", "Dot + Error")$yvar)
    expect_true("los_days" %in%
                  gs_selector_choices(info(), "quantile", "Dot + Error")$yvar)
  })
})

test_that("the error bars offered are the ones the Y variable can carry", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Dot + Error", yvar = "los_days",
                      xvar = "severity", strat_vars = character(),
                      jitter = FALSE)
    session$elapse(500)
    # A measurement has a mean and a spread; the proportion intervals are not
    # offered at all rather than offered and then refused.
    expect_setequal(gs_err_choices(info(), "los_days"),
                    c("se", "normal"))

    session$setInputs(yvar = "death")
    session$elapse(500)
    expect_setequal(gs_err_choices(info(), "death"), GS_ERR_PROP_TYPES)
  })
})

test_that("the tick labels can be turned, and the turn survives the theme", {
  shiny::testServer(gs_server(epi_cohort, "epi_cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Boxplot", yvar = "los_days",
                      xvar = "severity", strat_vars = character(),
                      jitter = FALSE, data_name = "epi_cohort")
    session$elapse(500)
    expect_false(grepl("axis.text.x", output$code, fixed = TRUE))

    session$setInputs(tick_angle_x = "45")
    session$elapse(500)
    expect_match(output$code, "angle = 45", fixed = TRUE)
    # After the theme, not before it: a theme replaces the element rather than
    # merging into it, so the order is what makes the setting stick.
    expect_lt(regexpr("theme_bw()", output$code, fixed = TRUE),
              regexpr("axis.text.x", output$code, fixed = TRUE))
    expect_no_error(output$plot)
  })
})

test_that("an outcome kept as a factor is described without being recoded first", {
  # Most people keep a binary outcome as a factor or as TRUE/FALSE. The figure
  # counts the value in the aes() rather than asking for the data to be changed.
  cohort <- data.table::as.data.table(epi_cohort)
  cohort[, outcome := factor(ifelse(death == 1L, "Died", "Survived"),
                             levels = c("Survived", "Died"))]

  shiny::testServer(gs_server(cohort, "cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Dot + Error", yvar = "outcome",
                      xvar = "severity", strat_vars = character(),
                      jitter = FALSE, data_name = "cohort")
    session$elapse(500)

    # It is offered as a Y variable, and only the proportion bars are offered
    # for it.
    expect_true("outcome" %in%
                  gs_selector_choices(info(), "quantile", "Dot + Error")$yvar)
    expect_setequal(gs_err_choices(info(), "outcome"), GS_ERR_PROP_TYPES)

    session$setInputs(err_type = "wilson")
    session$elapse(500)

    expect_length(problems(), 0L)
    # The second level is counted, and the code and the axis both say so.
    expect_equal(spec_r()$err_event, "Died")
    expect_match(output$code, 'y = as.integer(outcome == "Died")', fixed = TRUE)
    expect_match(output$code, 'labs(y = "Proportion outcome = Died")',
                 fixed = TRUE)
    expect_no_error(output$plot)

    # And the figure agrees with the same outcome written as 0 and 1.
    env <- new.env(parent = asNamespace("ggstratify"))
    assign("cohort", cohort, envir = env)
    bars <- ggplot2::ggplot_build(
      eval(parse(text = output$code), envir = env))$data[[1L]]
    ref <- vapply(split(cohort$death, cohort$severity), function(x)
      suppressWarnings(stats::prop.test(sum(x), length(x),
                                        correct = FALSE)$conf.int),
      numeric(2L))
    expect_equal(bars$y, unname(vapply(split(cohort$death, cohort$severity),
                                       mean, numeric(1L))))
    expect_equal(bars$ymin, unname(ref[1L, ]), tolerance = 1e-8)

    # Counting the other level is one control away.
    session$setInputs(err_event = "Survived")
    session$elapse(500)
    expect_equal(spec_r()$err_event, "Survived")
    expect_match(output$code, 'outcome == "Survived"', fixed = TRUE)
  })
})

test_that("a survey weight reaches the counts, the figure and the code", {
  cohort <- data.table::as.data.table(epi_cohort)
  set.seed(3)
  cohort[, svy_w := round(stats::runif(.N, 0.5, 40), 2)]
  cohort[1:4, svy_w := NA]

  shiny::testServer(gs_server(cohort, "cohort"), {
    do.call(session$setInputs, gs_test_inputs(tempdir()))
    session$setInputs(plot_type = "Histogram", yvar = "age", strat_vars = "sex",
                      jitter = FALSE, data_name = "cohort", weight = "svy_w")
    session$elapse(500)

    expect_length(problems(), 0L)
    expect_true("svy_w" %in% gs_selector_choices(info())$weight)
    # The rows with no weight are left out of every count, and said to be.
    miss <- missing_report()
    expect_equal(miss$n_missing[miss$var == "svy_w"], 4L)
    expect_equal(sum(strata()$n), nrow(cohort) - 4L)
    expect_equal(sum(strata()$n_w), sum(cohort$svy_w, na.rm = TRUE))
    expect_match(output$strata_table, "Weighted N", fixed = TRUE)

    expect_match(output$code, "aes(x = age, weight = svy_w)", fixed = TRUE)
    expect_match(output$code, "; weighted N = ", fixed = TRUE)
    expect_no_error(output$plot)

    session$setInputs(plot_type = "Dot + Error", yvar = "bmi",
                      xvar = "treatment", err_type = "normal")
    session$elapse(500)
    expect_length(problems(), 0L)
    expect_match(output$code, 'svy_summary(d, des, "bmi"', fixed = TRUE)
    expect_no_error(output$plot)

    # The strata reach the design, in the code and on screen.
    session$setInputs(design_strata = "site")
    session$elapse(500)
    expect_length(problems(), 0L)
    expect_match(output$code,
                 "survey::svydesign(ids = ~1, strata = ~site, weights = ~svy_w",
                 fixed = TRUE)
    expect_s3_class(svy_design(), "survey.design2")
    expect_no_error(output$plot)

    # A dotplot cannot be weighted, and says so rather than drawing unweighted.
    session$setInputs(plot_type = "Dotplot")
    session$elapse(500)
    expect_match(problems(), "cannot carry a survey weight", all = FALSE)
  })
})
