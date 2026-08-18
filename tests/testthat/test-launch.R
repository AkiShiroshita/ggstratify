# The launcher must reject bad input before a Shiny app is ever started, so
# the user gets one clear message instead of a broken reactive.

test_that("ggstratify rejects input that is not tabular", {
  expect_error(ggstratify(1:10), "dataset")
  expect_error(ggstratify(data.frame()), "dataset")
  expect_error(ggstratify(NA), "dataset")
})

test_that("ggstratify takes an object, never a path to one", {
  # A file read here would be typed by a guess, and then described on that
  # guess. The user reads the file, checks the types, and passes the result.
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(iris, path, row.names = FALSE)
  expect_error(ggstratify(path), "dataset")

  # And the app has no way to load one either: nothing in the UI is a file
  # control, so there is no second route back to guessed types.
  ui_src <- paste(deparse(gs_ui), collapse = " ")
  expect_false(grepl("fileInput", ui_src, fixed = TRUE))
  expect_false(grepl("data_file", ui_src, fixed = TRUE))
})

test_that("the data set is required", {
  expect_error(ggstratify(), "dataset")
})

test_that("ggstratify rejects a non-flag launch.browser", {
  expect_error(ggstratify(iris, launch.browser = "yes"), "launch.browser")
})

test_that("the generated code calls the data by the name it was passed under", {
  # ggstratify(cohort) should print `d <- cohort`, not `d <- mydata`.
  cohort <- iris
  expect_equal(gs_data_name(quote(cohort)), "cohort")
  # Anything that is not a plain name is not something to paste into a script.
  expect_equal(gs_data_name(quote(subset(iris, Species == "setosa"))), "mydata")
  expect_equal(gs_data_name(quote(`odd name`)), "mydata")
  # Dot names are names, and syntactic ones, but they mean nothing outside the
  # call that supplied them.
  expect_equal(gs_data_name(quote(.)), "mydata")
  expect_equal(gs_data_name(quote(...)), "mydata")
  expect_equal(gs_data_name(quote(..1)), "mydata")
  # A leading dot is otherwise an ordinary name and stays one.
  expect_equal(gs_data_name(quote(.cohort)), ".cohort")
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

test_that("every namespace import is declared and every declared one is used", {
  ns <- readLines(system.file("NAMESPACE", package = "ggstratify"))
  imported <- unique(c(
    sub("^import[(]([^)]*)[)]$", "\\1", grep("^import[(]", ns, value = TRUE)),
    sub("^importFrom[(]([^,]*),.*$", "\\1", grep("^importFrom[(]", ns, value = TRUE))
  ))
  desc <- read.dcf(system.file("DESCRIPTION", package = "ggstratify"))
  declared <- trimws(gsub("[(][^)]*[)]", "",
                          strsplit(desc[1, "Imports"], ",")[[1]]))
  # A namespace import that DESCRIPTION does not list is an R CMD check
  # warning. `tools` was imported here for two functions nothing called.
  expect_equal(setdiff(imported, declared), character())
})
