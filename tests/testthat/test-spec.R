test_that("gs_spec fills defaults and keeps overrides", {
  spec <- gs_spec(plot_type = "Violin", y = "bmi")
  expect_equal(spec$plot_type, "Violin")
  expect_equal(spec$y, "bmi")
  expect_equal(spec$theme, "theme_bw()")
  expect_equal(spec$outdir, "figures")
  expect_equal(spec$strat_vars, character())
})

test_that("gs_spec_cols lists only the columns the plot reads", {
  spec <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bmi",
                  group = "arm", facet = "site", strat_vars = "sex")
  # Deduplicated, and the stratifying variable is handled separately.
  expect_setequal(gs_spec_cols(spec), c("arm", "bmi", "site"))
})

test_that("validation catches missing variables per plot type", {
  expect_match(gs_validate_spec(gs_spec(plot_type = "Boxplot")),
               "needs a Y-variable")
  expect_match(gs_validate_spec(gs_spec(plot_type = "Density")),
               "needs a continuous variable")
  expect_match(gs_validate_spec(gs_spec(plot_type = "Scatter", x = "a")),
               "both an X- and a Y-variable")

  expect_length(gs_validate_spec(gs_spec(plot_type = "Boxplot", y = "bmi")), 0L)
})

test_that("validation warns when a categorical variable is used as continuous", {
  info <- gs_classify_vars(gs_prepare_data(iris))
  ok <- gs_validate_spec(gs_spec(plot_type = "Boxplot", x = "Species",
                                 y = "Sepal.Length"), info)
  expect_length(ok, 0L)

  bad <- gs_validate_spec(gs_spec(plot_type = "Boxplot", y = "Species"), info)
  expect_match(bad, "does not look continuous")
})

test_that("gs_num falls back on unusable input", {
  expect_equal(gs_num(3), 3)
  expect_equal(gs_num("2.5"), 2.5)
  expect_equal(gs_num(NULL, 7), 7)
  expect_equal(gs_num("abc", 7), 7)
  expect_equal(gs_num(NA, 7), 7)
})

test_that("an axis range that runs backwards is reported", {
  spec <- gs_spec(plot_type = "Boxplot", y = "a", xlim_min = 100,
                  xlim_max = 10)
  expect_match(gs_validate_spec(spec), "X-axis range runs backwards", all = FALSE)

  spec2 <- gs_spec(plot_type = "Boxplot", y = "a", ylim_min = 5, ylim_max = 5)
  expect_match(gs_validate_spec(spec2), "Y-axis range runs backwards", all = FALSE)

  # One end alone means "from here"/"up to here" and is not a problem.
  spec3 <- gs_spec(plot_type = "Boxplot", y = "a", xlim_min = 10)
  expect_false(any(grepl("runs backwards", gs_validate_spec(spec3))))

  spec4 <- gs_spec(plot_type = "Boxplot", y = "a", xlim_min = 10, xlim_max = 100)
  expect_false(any(grepl("runs backwards", gs_validate_spec(spec4))))
})

test_that("splitting a variable by its own missingness is spotted", {
  cut <- gs_cut("bmi", method = "missing")

  # bmi is drawn, and the figures are split by whether bmi is there: the
  # Missing figure is drawn from rows that are all NA.
  spec <- gs_normalize_spec(gs_spec(plot_type = "Boxplot", y = "bmi",
                                    x = "sex", strat_vars = "bmi_missing",
                                    cuts = list(cut)))
  found <- gs_self_missing_cuts(spec)
  expect_length(found, 1L)
  expect_equal(found[[1]]$new, "bmi_missing")
  expect_match(as.character(gs_self_missing_alert(found)), "no bmi to draw")

  # The useful arrangement -- describing something else within the same split
  # -- is not flagged.
  ok <- gs_normalize_spec(gs_spec(plot_type = "Boxplot", y = "age", x = "sex",
                                  strat_vars = "bmi_missing",
                                  cuts = list(cut)))
  expect_length(gs_self_missing_cuts(ok), 0L)
  expect_null(gs_self_missing_alert(gs_self_missing_cuts(ok)))

  # Nor is it flagged when nothing is split by it at all.
  unused <- gs_normalize_spec(gs_spec(plot_type = "Boxplot", y = "bmi",
                                      cuts = list(cut)))
  expect_length(gs_self_missing_cuts(unused), 0L)
})

test_that("the self-missingness check reads every role the figure draws from", {
  # A variable can reach the figure as the panel colour or as a Kaplan-Meier
  # axis, not only as x or y, and each of those is emptied by its own
  # missingness just the same.
  grouped <- gs_normalize_spec(gs_spec(plot_type = "Scatter", x = "age",
                                       y = "los_days", group = "crp",
                                       facet = "crp_missing",
                                       cuts = list(gs_cut("crp", method = "missing"))))
  expect_length(gs_self_missing_cuts(grouped), 1L)

  km <- gs_normalize_spec(gs_spec(plot_type = GS_KM, time = "fu_days",
                                  event = "death",
                                  strat_vars = "fu_days_missing",
                                  cuts = list(gs_cut("fu_days", method = "missing"))))
  expect_length(gs_self_missing_cuts(km), 1L)

  # A plot type that does not read the column is not warned about: x is
  # cleared for a Kaplan-Meier curve, so bmi is not drawn here.
  cleared <- gs_normalize_spec(gs_spec(plot_type = GS_KM, time = "fu_days",
                                       event = "death", x = "bmi",
                                       strat_vars = "bmi_missing",
                                       cuts = list(gs_cut("bmi", method = "missing"))))
  expect_length(gs_self_missing_cuts(cleared), 0L)
})
