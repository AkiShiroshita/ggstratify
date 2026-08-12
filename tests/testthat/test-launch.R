# The launcher must reject bad input before a Shiny app is ever started, so
# the user gets one clear message instead of a broken reactive.

test_that("ggstratify rejects input that is not tabular and not a file", {
  expect_error(ggstratify(1:10), "dataset")
  expect_error(ggstratify("no-such-file.csv"), "dataset")
  expect_error(ggstratify(data.frame()), "dataset")
})

test_that("ggstratify rejects a non-flag launch.browser", {
  expect_error(ggstratify(iris, launch.browser = "yes"), "launch.browser")
})

test_that("ggstratify accepts ggplot_shiny's NA default for no data", {
  # NA must be read as "no data yet", not as an unusable dataset. Reaching the
  # launch.browser check is what shows NA got that far.
  expect_error(ggstratify(NA, launch.browser = "not a flag"), "launch.browser")
})

test_that("there is exactly one exported function to learn", {
  ns <- readLines(system.file("NAMESPACE", package = "ggstratify"))
  expect_equal(grep("^export\\(", ns, value = TRUE), "export(ggstratify)")
  # The old second entry point is gone, not merely unexported.
  expect_false(exists("ggstratify_classic", envir = asNamespace("ggstratify"),
                      inherits = FALSE))
})

test_that("the package does not depend on ggplotgui", {
  deps <- unlist(strsplit(
    paste(utils::packageDescription("ggstratify",
                                    fields = c("Depends", "Imports", "LinkingTo")),
          collapse = ", "),
    ",\\s*"
  ))
  expect_false(any(grepl("ggplotgui", deps)))
})
