km_spec <- function(...) {
  gs_spec(plot_type = GS_KM, time = "fu_days", event = "death", ...)
}

cohort <- function() gs_prepare_data(epi_cohort)

test_that("the Kaplan-Meier spec drops any lingering x and y selection", {
  spec <- km_spec(x = "sex", y = "bmi")
  expect_equal(spec$x, "")
  expect_equal(spec$y, "")
  # The variables it does read are the ones it is narrowed to.
  expect_setequal(gs_spec_cols(km_spec(group = "sex")),
                  c("fu_days", "death", "sex"))
})

test_that("a curve is drawable, with and without a grouping variable", {
  dt <- cohort()
  for (spec in list(km_spec(),
                    km_spec(group = "treatment"),
                    km_spec(group = "treatment", km_ci = TRUE, palette = "Set1"),
                    km_spec(km_censor = FALSE, km_ylim = FALSE))) {
    code <- gs_code_figure(spec, "d")
    expect_silent(parse(text = paste(code, collapse = "\n")))
    expect_no_error(ggplot2::ggplot_build(gs_eval_plot(code, dt)))
  }
})

test_that("the fit is computed within everything that splits the figure", {
  # A curve fitted across panels and then drawn in one of them would be the
  # wrong curve, so grouping and faceting both enter the `by`.
  expect_equal(gs_km_by(km_spec()), character())
  expect_equal(gs_km_by(km_spec(group = "sex")), "sex")
  # With the sizes on the strips, the fit is split by the labelled copy of
  # the facet column -- the same column the panels come from.
  expect_equal(gs_km_by(km_spec(group = "sex", facet = "site")),
               c("sex", ".facet_label"))
  expect_equal(gs_km_by(km_spec(group = "sex", facet = "site", show_n = FALSE)),
               c("sex", "site"))
  expect_equal(gs_km_by(km_spec(group = "sex"), facet_strata = TRUE),
               c("sex", ".strat_label"))

  prep <- gs_code_prep(km_spec(group = "sex"), "d")
  expect_match(prep[1], 'km_data(d, "fu_days", "death", by = c("sex"))',
               fixed = TRUE)
  expect_match(gs_code_prep(km_spec(), "d")[1],
               'km_data(d, "fu_days", "death")', fixed = TRUE)
})

test_that("the all-strata preview fits one curve per panel", {
  dt <- cohort()
  spec <- km_spec(group = "treatment", strat_vars = c("sex", "site"))
  long <- gs_long_strata(dt, spec$strat_vars, gs_spec_cols(spec), 10L)

  code <- gs_code_figure(spec, "d", facet_strata = TRUE)
  expect_match(paste(code, collapse = "\n"), '"treatment", ".strat_label"',
               fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(gs_eval_plot(code, long)))
})

test_that("the confidence band and the censoring marks are opt-in layers", {
  plain <- paste(gs_code_plot(km_spec()), collapse = "\n")
  expect_false(grepl("geom_ribbon", plain, fixed = TRUE))
  expect_match(plain, "geom_point(data = function(x) x[x$.ncens > 0, ]",
               fixed = TRUE)

  ci <- paste(gs_code_plot(km_spec(km_ci = TRUE, km_censor = FALSE)),
              collapse = "\n")
  expect_match(ci, "geom_ribbon(aes(ymin = .lower, ymax = .upper)", fixed = TRUE)
  expect_false(grepl("geom_point", ci, fixed = TRUE))
})

test_that("a grouped band colours both aesthetics from one palette", {
  code <- paste(gs_code_plot(km_spec(group = "sex", km_ci = TRUE,
                                     palette = "Set1", lab_legend = "Sex")),
                collapse = "\n")
  expect_match(code, "colour = sex, fill = sex", fixed = TRUE)
  expect_match(code, "scale_colour_brewer(palette = \"Set1\")", fixed = TRUE)
  expect_match(code, "scale_fill_brewer(palette = \"Set1\")", fixed = TRUE)
  # One renamed legend, not one renamed and one not.
  expect_match(code, 'colour = "Sex", fill = "Sex"', fixed = TRUE)
})

test_that("the axes are named, because .time and .surv are not names", {
  code <- paste(gs_code_plot(km_spec()), collapse = "\n")
  expect_match(code, 'labs(x = "Time", y = "Survival probability")', fixed = TRUE)

  # ...unless the user named them.
  mine <- paste(gs_code_plot(km_spec(lab_x = "Days", lab_y = "Alive")),
                collapse = "\n")
  expect_match(mine, 'labs(x = "Days", y = "Alive")', fixed = TRUE)
})

test_that("the y axis is clamped to 0-1 unless that is switched off", {
  expect_match(paste(gs_code_plot(km_spec()), collapse = "\n"),
               "coord_cartesian(ylim = c(0, 1))", fixed = TRUE)
  expect_false(grepl("coord_cartesian",
                     paste(gs_code_plot(km_spec(km_ylim = FALSE)), collapse = "\n"),
                     fixed = TRUE))

  # 0 to 1 is a default, and a range typed into the sidebar replaces it.
  typed <- paste(gs_code_plot(km_spec(ylim_min = 0.5)), collapse = "\n")
  expect_match(typed, "coord_cartesian(ylim = c(0.5, NA))", fixed = TRUE)
  expect_false(grepl("ylim = c(0, 1)", typed, fixed = TRUE))
})

test_that("the risk table is counted across the range the curve is shown over", {
  code <- paste(gs_code_figure(km_spec(km_risk = TRUE, xlim_max = 60), "d"),
                collapse = "\n")
  expect_match(code, "risk_max <- 60", fixed = TRUE)
  expect_match(code, "risk_min <- 0", fixed = TRUE)
  # The shared scale already carries the range, so the curve does not repeat
  # it as a coord_cartesian() the table would not have.
  expect_false(grepl("xlim = ", code, fixed = TRUE))

  auto <- paste(gs_code_figure(km_spec(km_risk = TRUE), "d"), collapse = "\n")
  expect_match(auto, "risk_max <- max(km$.time, na.rm = TRUE)", fixed = TRUE)
})

test_that("a curve is never drawn from a subsample", {
  # Sampling a distribution keeps its shape; sampling a survival curve makes
  # a different, wider curve.
  expect_equal(gs_sample_threshold(km_spec()), Inf)
  # An infinite cap is not reached by any table, so a small one shows it: the
  # threshold gs_downsample() takes by default is the spec's, and it is Inf.
  n <- 1000L
  d <- data.table::data.table(t = runif(n), e = rbinom(n, 1, 0.5))
  expect_identical(gs_downsample(d, km_spec(), TRUE), d)
  expect_lt(nrow(gs_downsample(d, km_spec(), TRUE, threshold = 100L)), n)
})

test_that("validation reports what a curve is missing", {
  info <- gs_classify_vars(cohort())
  expect_match(gs_validate_spec(gs_spec(plot_type = GS_KM)),
               "needs a time variable", all = FALSE)
  expect_match(gs_validate_spec(gs_spec(plot_type = GS_KM,
                                        time = "fu_days")),
               "needs an event variable")
  expect_length(gs_validate_spec(km_spec(), info), 0L)

  # Surv() takes 0/1, 1/2 or logical, and nothing else.
  bad <- gs_validate_spec(gs_spec(plot_type = GS_KM, time = "fu_days",
                                  event = "severity"), info)
  expect_match(bad, "must be 0/1, 1/2 or TRUE/FALSE")

  bad_time <- gs_validate_spec(gs_spec(plot_type = GS_KM, time = "sex",
                                       event = "death"), info)
  expect_match(bad_time, "time variable must be numeric")
})

test_that("gs_is_event_col accepts exactly what Surv() accepts", {
  expect_true(gs_is_event_col(c(0L, 1L, 1L)))
  expect_true(gs_is_event_col(c(1, 2, 2)))
  expect_true(gs_is_event_col(c(TRUE, FALSE, NA)))
  expect_true(gs_is_event_col(c(0, 0, 0)))
  expect_false(gs_is_event_col(c(0, 1, 2)))
  expect_false(gs_is_event_col(factor(c("yes", "no"))))
  expect_false(gs_is_event_col(c("0", "1")))
})

test_that("a column called time is not confused with the helper's argument", {
  dt <- cohort()
  data.table::setnames(dt, "fu_days", "time")
  code <- gs_code_figure(gs_spec(plot_type = GS_KM, time = "time",
                                 event = "death", group = "sex"), "d")
  expect_no_error(ggplot2::ggplot_build(gs_eval_plot(code, dt)))
})

test_that("the columns the fit produces cannot be shadowed by the data", {
  dt <- data.table::data.table(.time = 1:3, .surv = 1:3, x = 1:3)
  expect_equal(names(gs_clean_names(dt)), c(".time_", ".surv_", "x"))
})

test_that("a stratum in which nobody had the event still draws", {
  dt <- cohort()
  dt[sex == "Male", death := 0L]
  code <- gs_code_figure(km_spec(group = "sex", km_ci = TRUE), "d")
  expect_no_error(ggplot2::ggplot_build(gs_eval_plot(code, dt)))
})

test_that("the generated code carries the helper it needs and runs", {
  spec <- km_spec(group = "treatment", data_name = "epi_cohort")
  script <- gs_code_script(spec)

  expect_silent(parse(text = script))
  expect_equal(length(gregexpr("km_data <- function", script)[[1]]), 1L)
  expect_match(script, "survival::survfit", fixed = TRUE)
  # It stands on its own: it calls survival and data.table, never ggstratify.
  expect_false(grepl("ggstratify::", script, fixed = TRUE))

  env <- new.env(parent = globalenv())
  p <- eval(parse(text = script), envir = env)
  expect_s3_class(p, "ggplot")
})

# --- the number-at-risk table -------------------------------------------------

test_that("the risk table is opt-in, and only for a survival curve", {
  plain <- paste(gs_code_figure(km_spec(), "d"), collapse = "\n")
  expect_false(grepl("km_risk", plain, fixed = TRUE))

  # The control belongs to one plot type, so it cannot reach another.
  box <- gs_spec(plot_type = "Boxplot", y = "bmi", km_risk = TRUE)
  expect_false(box$km_risk)
  expect_false(grepl("km_risk", paste(gs_code_figure(box, "d"), collapse = "\n"),
                     fixed = TRUE))
})

test_that("the risk table is counted at the times the curve is labelled at", {
  code <- paste(gs_code_figure(km_spec(group = "treatment", km_risk = TRUE), "d"),
                collapse = "\n")

  expect_match(code, "km_risk <- function", fixed = TRUE)
  expect_match(code, "risk <- km_risk(d, \"fu_days\", \"death\", risk_times",
               fixed = TRUE)
  # One set of breaks, given to both panels: a count under the wrong point of
  # the curve is worse than no count at all.
  expect_equal(
    length(gregexpr(
      "scale_x_continuous(limits = c(risk_min, risk_max), breaks = risk_times)",
      code, fixed = TRUE)[[1]]), 2L)
  # The curve drops the axis the table underneath carries for both.
  expect_match(code, "axis.text.x = element_blank()", fixed = TRUE)
  expect_match(code, "patchwork::wrap_plots(p, tbl", fixed = TRUE)
})

test_that("the risk table draws, grouped, ungrouped and panelled", {
  dt <- cohort()
  for (spec in list(
    km_spec(km_risk = TRUE),
    km_spec(km_risk = TRUE, group = "treatment"),
    km_spec(km_risk = TRUE, group = "treatment", facet = "sex"),
    km_spec(km_risk = TRUE, km_ci = TRUE, group = "sex", palette = "Set1")
  )) {
    code <- gs_code_figure(spec, "d")
    expect_silent(parse(text = paste(code, collapse = "\n")))
    p <- gs_eval_plot(code, dt)
    expect_s3_class(p, "patchwork")
    # Building the assembly is what actually exercises both halves of it.
    expect_no_error(patchwork::patchworkGrob(p))
  }
})

test_that("the counts in the table are the numbers still being followed", {
  dt <- cohort()
  code <- gs_code_figure(km_spec(km_risk = TRUE), "d")
  env <- new.env(parent = asNamespace("ggstratify"))
  assign("d", dt, envir = env)
  eval(parse(text = paste(code, collapse = "\n")), envir = env)
  risk <- get("risk", envir = env)

  # Everyone is at risk at time zero, and nobody is added later.
  expect_equal(risk$.nrisk[risk$.time == 0], nrow(dt))
  expect_true(all(diff(risk$.nrisk) <= 0))
})
