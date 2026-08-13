cohort <- function() gs_prepare_data(epi_cohort)

test_that("gs_cut fills in a name and tidies the cut points", {
  cut <- gs_cut("age")
  expect_equal(cut$new, "age_cat")
  expect_equal(cut$method, "quantile")
  expect_equal(cut$n, 4L)

  # Unsorted, duplicated and non-finite cut points are all dealt with here so
  # that nothing downstream has to.
  expect_equal(gs_cut("age", method = "breaks",
                      breaks = c(80, 65, 65, NA, Inf))$breaks,
               c(65, 80))
  expect_equal(gs_cut("age", n = 1)$n, 2L)
})

test_that("cut points are read from what the user typed", {
  expect_equal(gs_parse_breaks("50, 65 ; 80"), c(50, 65, 80))
  expect_equal(gs_parse_breaks(" 65 "), 65)
  expect_equal(gs_parse_breaks(""), numeric())
  expect_true(anyNA(gs_parse_breaks("50, abc")))
})

test_that("each method produces the categorisation it promises", {
  dt <- cohort()
  out <- gs_apply_cuts(dt, list(
    gs_cut("age", method = "quantile", n = 4),
    gs_cut("bmi", new = "bmi_bin", method = "equal", n = 3),
    gs_cut("age", new = "age65", method = "breaks", breaks = 65)
  ))

  expect_equal(nlevels(out$age_cat), 4L)
  # Quantile groups are equal-sized to within a tie.
  expect_lt(diff(range(table(out$age_cat))), nrow(dt) * 0.05)

  expect_equal(nlevels(out$bmi_bin), 3L)
  expect_equal(levels(out$age65), c("(-Inf,65]", "(65, Inf]"))
  expect_equal(sum(out$age65 == "(-Inf,65]"), sum(dt$age <= 65))
})

test_that("categorising never touches the data it was given", {
  dt <- cohort()
  before <- names(dt)
  out <- gs_apply_cuts(dt, list(gs_cut("age")))
  expect_equal(names(dt), before)
  expect_true("age_cat" %in% names(out))
})

test_that("a derived variable is categorical and can be stratified on", {
  out <- gs_apply_cuts(cohort(), list(gs_cut("age", method = "breaks",
                                             breaks = 65)))
  info <- gs_classify_vars(out)
  expect_true(info[var == "age_cat", is_categorical])
  expect_true(info[var == "age_cat", can_stratify])
  expect_true("age_cat" %in% gs_vars_of(info, "stratify"))
})

test_that("later rules can read a column an earlier rule created", {
  # ...but a rule whose source is missing, or whose name is taken, is dropped.
  cuts <- list(gs_cut("age", new = "age_cat"),
               gs_cut("nope", new = "x"),
               gs_cut("bmi", new = "age"))
  kept <- gs_valid_cuts(cuts, names(cohort()))
  expect_length(kept, 1L)
  expect_equal(kept[[1]]$new, "age_cat")
})

test_that("gs_check_cut refuses what cannot work, before it is added", {
  dt <- cohort()
  expect_match(gs_check_cut(dt, gs_cut("sex")), "not numeric")
  expect_match(gs_check_cut(dt, gs_cut("nope")), "not a column")
  expect_match(gs_check_cut(dt, gs_cut("age", new = "sex")), "already exists")
  expect_match(gs_check_cut(dt, gs_cut("age", method = "breaks")),
               "at least one cut point")
  expect_match(gs_check_cut(dt, gs_cut("age", n = 99)), "between 2 and 20")
  # A binary column has no four quantile groups to give.
  expect_match(gs_check_cut(dt, gs_cut("death", n = 4)), "fewer than two")
  expect_length(gs_check_cut(dt, gs_cut("age", n = 3)), 0L)
})

test_that("a rule that fails is skipped rather than breaking the app", {
  # cut() refuses a character column. gs_check_cut() stops this reaching the
  # data in the app; if one ever does, the column is dropped and the message
  # kept rather than the whole table failing to build.
  dt <- data.table::data.table(x = letters[1:5])
  out <- gs_apply_cuts(dt, list(gs_cut("x", method = "breaks", breaks = 2)))
  expect_false("x_cat" %in% names(out))
  expect_match(attr(out, "gs_cut_error"), "x_cat")
})

test_that("the generated lines are the ones the app itself runs", {
  cuts <- list(gs_cut("age", method = "quantile", n = 3))
  lines <- gs_code_cuts(cuts, "dt")
  expect_match(lines[2], "dt[, age_cat := cut(age, breaks = unique(stats::quantile(",
               fixed = TRUE)
  expect_silent(parse(text = paste(lines, collapse = "\n")))

  # Running the emitted line by hand gives the app's column, value for value.
  dt <- cohort()
  by_app <- gs_apply_cuts(dt, cuts)$age_cat
  by_hand <- data.table::copy(dt)
  eval(parse(text = lines[2]), envir = list2env(list(dt = by_hand)))
  expect_equal(by_app, by_hand$age_cat)
})

test_that("awkward variable names survive the round trip", {
  dt <- data.table::data.table(`my age` = as.numeric(1:100))
  cut <- gs_cut("my age", new = "my group", method = "breaks", breaks = 50)
  expect_length(gs_check_cut(dt, cut), 0L)
  out <- gs_apply_cuts(dt, list(cut))
  expect_true("my group" %in% names(out))
  expect_equal(as.integer(table(out[["my group"]])), c(50L, 50L))
})

test_that("the code creates the derived columns before it uses them", {
  spec <- gs_spec(plot_type = "Boxplot", x = "sex", y = "los_days",
                  cuts = list(gs_cut("age", method = "breaks", breaks = 65)),
                  strat_vars = "age_cat", data_name = "epi_cohort")
  row <- data.table::data.table(var = "age_cat", level = "(65, Inf]",
                                label = "age_cat: (65, Inf]",
                                file = "age_cat_65_Inf", n = 200L, keep = TRUE)
  script <- gs_code_script(spec, row)

  expect_silent(parse(text = script))
  expect_match(script, "dt[, age_cat := cut(age, breaks = c(-Inf, 65, Inf))]",
               fixed = TRUE)
  # Created straight after the data is read, and before the subset reads it.
  expect_lt(regexpr("age_cat :=", script, fixed = TRUE),
            regexpr("d <- dt[", script, fixed = TRUE))
  # ...and the whole thing runs, which is the point of the ordering.
  env <- new.env(parent = globalenv())
  assign("epi_cohort", epi_cohort, envir = env)
  expect_s3_class(eval(parse(text = script), envir = env), "ggplot")
})

test_that("gs_spec keeps the rules it is handed", {
  # modifyList() would quietly drop an unnamed list of rules.
  spec <- gs_spec(cuts = list(gs_cut("age"), gs_cut("bmi")))
  expect_length(spec$cuts, 2L)
  expect_equal(spec$cuts[[2]]$new, "bmi_cat")

  # A bare rule, not wrapped in a list, is understood too.
  expect_length(gs_spec(cuts = gs_cut("age"))$cuts, 1L)
  expect_length(gs_spec()$cuts, 0L)
})

test_that("gs_cut_name avoids names the data already uses", {
  expect_equal(gs_cut_name("age", c("bmi")), "age_cat")
  expect_equal(gs_cut_name("age", c("age_cat")), "age_cat2")
  expect_equal(gs_cut_name("age", c("age_cat", "age_cat2")), "age_cat3")
})

test_that("a rule describes itself for the sidebar", {
  expect_equal(gs_cut_describe(gs_cut("age", n = 4)),
               "age_cat <- age (4 quantile groups)")
  expect_equal(gs_cut_describe(gs_cut("age", method = "equal", n = 3)),
               "age_cat <- age (3 equal-width bins)")
  expect_equal(gs_cut_describe(gs_cut("age", method = "breaks",
                                      breaks = c(50, 65))),
               "age_cat <- age (cut at 50, 65)")
})
