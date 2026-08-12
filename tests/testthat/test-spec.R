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
