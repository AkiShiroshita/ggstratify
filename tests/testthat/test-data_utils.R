test_that("gs_prepare_data never modifies the caller's object", {
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  dt <- gs_prepare_data(df)

  expect_true(data.table::is.data.table(dt))
  expect_false(data.table::is.data.table(df))

  # Writing by reference into the result must not reach back into `df`.
  dt[, c := a * 2]
  expect_named(df, c("a", "b"))
})

test_that("gs_prepare_data copies data.tables too", {
  src <- data.table::data.table(a = 1:3)
  out <- gs_prepare_data(src)
  out[, b := 1L]
  expect_named(src, "a")
})

test_that("gs_prepare_data accepts a tibble and drops its subclass", {
  skip_if_not_installed("tibble")
  src <- tibble::as_tibble(data.frame(a = 1:3, b = letters[1:3]))
  out <- gs_prepare_data(src)

  # A plain data.table, not a tbl_df wearing a data.table hat: the generated
  # code is written against data.table alone.
  expect_identical(class(out), c("data.table", "data.frame"))
  out[, a := 99L]
  expect_identical(src$a, 1:3)
})

test_that("gs_prepare_data keeps every row of a subclassed data.frame", {
  # Stands in for a grouped tibble: a grouping carried on the class is not a
  # stratification, so it is dropped and no row is summarised away.
  src <- data.frame(g = c("a", "a", "b"), y = 1:3)
  class(src) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out <- gs_prepare_data(src)

  expect_identical(class(out), c("data.table", "data.frame"))
  expect_equal(nrow(out), 3L)
})

test_that("gs_prepare_data rejects non-tabular input", {
  expect_error(gs_prepare_data(1:10), "data.frame")
})

test_that("gs_prepare_data does not read files", {
  # A path is not data. Reading one here would type every column by guess,
  # and the app would then describe the data on those guesses.
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(iris, path, row.names = FALSE)
  expect_error(gs_prepare_data(path), "data.frame")
})

test_that("gs_clean_names fixes blank, duplicated and reserved names", {
  dt <- data.table::data.table(1:2, 1:2, 1:2)
  data.table::setnames(dt, c("x", "x", ".strat_label"))
  out <- gs_clean_names(dt)
  expect_equal(names(out), c("x", "x_1", ".strat_label_"))
})

test_that("gs_classify_vars separates continuous from categorical", {
  dt <- gs_prepare_data(iris)
  info <- gs_classify_vars(dt)

  expect_true(info[var == "Sepal.Length", is_continuous])
  expect_false(info[var == "Species", is_continuous])
  expect_true(info[var == "Species", can_stratify])
  expect_equal(info[var == "Species", n_levels], 3L)

  expect_equal(gs_vars_of(info, "stratify"), "Species")
  expect_true("Sepal.Width" %in% gs_vars_of(info, "continuous"))
})

test_that("a coded numeric with few levels counts as categorical", {
  dt <- data.table::data.table(grp = rep(1:3, each = 20), y = rnorm(60))
  info <- gs_classify_vars(dt)
  expect_true(info[var == "grp", is_categorical])
  expect_true(info[var == "grp", can_stratify])
  expect_true(info[var == "y", is_continuous])
})

test_that("high-cardinality variables are not offered for stratification", {
  dt <- data.table::data.table(id = as.character(1:200))
  info <- gs_classify_vars(dt)
  expect_true(info[var == "id", is_categorical])
  expect_false(info[var == "id", can_stratify])
})

test_that("gs_stratify_choices labels variables with their level count", {
  info <- gs_classify_vars(gs_prepare_data(iris))
  ch <- gs_stratify_choices(info)
  expect_equal(unname(ch), "Species")
  expect_match(names(ch), "Species\\s+\\(3\\)")
})

test_that("gs_strata_table treats each variable independently", {
  dt <- data.table::data.table(
    sex = rep(c("M", "F"), each = 30),
    arm = rep(c("A", "B", "C"), 20),
    y = rnorm(60)
  )
  st <- gs_strata_table(dt, c("sex", "arm"), min_n = 0L)

  expect_equal(nrow(st), 5L)                     # 2 + 3, not 2 * 3
  expect_setequal(st$label,
                  c("sex: F", "sex: M", "arm: A", "arm: B", "arm: C"))
  expect_equal(st[var == "sex", sum(n)], 60L)
  expect_equal(st[var == "arm", sum(n)], 60L)
  expect_true(all(st$file %in% c("sex_F", "sex_M", "arm_A", "arm_B", "arm_C")))
})

test_that("gs_strata_table flags strata below min_n", {
  dt <- data.table::data.table(g = c(rep("big", 50), "small"), y = rnorm(51))
  st <- gs_strata_table(dt, "g", min_n = 10L)
  expect_equal(st[level == "small", keep], FALSE)
  expect_match(st[level == "small", status], "n < 10")
  expect_equal(st[level == "big", keep], TRUE)
  expect_equal(st[level == "big", status], "exported")
})

test_that("an unused factor level is reported with n = 0", {
  dt <- data.table::data.table(
    g = factor(c("a", "a", "b"), levels = c("a", "b", "empty")),
    y = rnorm(3)
  )
  st <- gs_strata_table(dt, "g", min_n = 0L)

  # The empty level is listed, not dropped.
  expect_equal(nrow(st), 3L)
  expect_equal(st[level == "empty", n], 0L)
  expect_false(st[level == "empty", keep])
  expect_match(st[level == "empty", status], "n = 0")
  # Levels that do have rows are still exported.
  expect_true(all(st[level != "empty", keep]))
})

test_that("n = 0 is skipped even when min_n allows everything", {
  dt <- data.table::data.table(
    g = factor(c("a", "b"), levels = c("a", "b", "empty")),
    y = rnorm(2)
  )
  st <- gs_strata_table(dt, "g", min_n = 0L)
  expect_false(st[level == "empty", keep])
  expect_true(all(st[n > 0L, keep]))
})

test_that("character columns cannot produce an n = 0 stratum", {
  dt <- data.table::data.table(g = c("a", "a", "b"), y = rnorm(3))
  st <- gs_strata_table(dt, "g", min_n = 0L)
  expect_equal(nrow(st), 2L)
  expect_true(all(st$n > 0L))
})

test_that("gs_label_n appends the stratum size", {
  expect_equal(gs_label_n("sex: M", 30), "sex: M (N = 30)")
  expect_equal(gs_label_n("site_D", 0), "site_D (N = 0)")
})

test_that("a cleared minimum n does not poison the size rules", {
  dt <- data.table::data.table(g = c("a", "b"), y = 1:2)
  st <- gs_strata_table(dt, "g", min_n = NA_integer_)
  expect_true(all(st$keep))
})

test_that("gs_strata_table returns an empty table for no stratifying variables", {
  dt <- gs_prepare_data(iris)
  expect_equal(nrow(gs_strata_table(dt, character())), 0L)
  expect_equal(nrow(gs_strata_table(dt, "not_a_column")), 0L)
})

test_that("gs_safe_name sanitises anything usable as a file name", {
  expect_equal(gs_safe_name("sex: M"), "sex_M")
  expect_equal(gs_safe_name("a/b\\c"), "a_b_c")
  expect_equal(gs_safe_name("keep.me-1"), "keep.me-1")
  expect_equal(gs_safe_name("***"), "NA")
})

test_that("gs_long_strata stacks one copy per stratifying variable", {
  dt <- data.table::data.table(
    sex = rep(c("M", "F"), each = 30),
    arm = rep(c("A", "B", "C"), 20),
    y = rnorm(60)
  )
  long <- gs_long_strata(dt, c("sex", "arm"), plot_cols = "y", min_n = 0L)

  expect_equal(nrow(long), 120L)                 # 60 rows per variable
  expect_setequal(names(long), c("y", ".strat_label"))
  expect_equal(nlevels(long$.strat_label), 5L)
})

test_that("the facet panels follow the level order, not the label spelling", {
  # The labels a categorized variable produces sort the wrong way as text:
  # "[0.5,3.9]" comes after "(3.9,8.13]".
  dt <- data.table::data.table(y = rnorm(60))
  dt[, band := cut(seq_len(60), breaks = c(0, 20, 40, 60),
                   labels = c("[0,20]", "(20,40]", "(40,60]"))]
  long <- gs_long_strata(dt, "band", plot_cols = "y", min_n = 0L)

  expect_equal(levels(long$.strat_label),
               c("band: [0,20] (N = 20)", "band: (20,40] (N = 20)",
                 "band: (40,60] (N = 20)"))
})

test_that("gs_long_strata drops small strata and leaves the source untouched", {
  dt <- data.table::data.table(g = c(rep("big", 50), "small"), y = rnorm(51))
  long <- gs_long_strata(dt, "g", plot_cols = "y", min_n = 10L)

  expect_equal(nrow(long), 50L)
  expect_named(dt, c("g", "y"))                  # no .strat_label leaked back
})

test_that("gs_split_strata mirrors the generated script's split", {
  dt <- data.table::data.table(
    sex = rep(c("M", "F"), each = 30),
    arm = rep(c("A", "B", "C"), 20),
    bmi = rnorm(60),
    unused = rnorm(60)
  )
  spec <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bmi",
                  strat_vars = c("sex", "arm"), min_n = 0L)
  out <- gs_split_strata(dt, spec)

  expect_named(out, c("sex_F", "sex_M", "arm_A", "arm_B", "arm_C"))
  expect_equal(vapply(out, nrow, integer(1L)),
               c(sex_F = 30L, sex_M = 30L, arm_A = 20L, arm_B = 20L, arm_C = 20L))
  # Columns are narrowed to what the plot reads, plus the stratifying variable.
  expect_setequal(names(out$sex_M), c("arm", "bmi", "sex"))
})

test_that("gs_split_strata applies min_n", {
  dt <- data.table::data.table(g = c(rep("big", 50), "small"), y = rnorm(51))
  spec <- gs_spec(plot_type = "Boxplot", y = "y", strat_vars = "g", min_n = 10L)
  expect_named(gs_split_strata(dt, spec), "g_big")
})

test_that("gs_split_strata exposes then removes empty factor levels", {
  dt <- data.table::data.table(
    g = factor(c("a", "a", "b"), levels = c("a", "b", "empty")),
    y = rnorm(3)
  )
  spec <- gs_spec(plot_type = "Boxplot", y = "y", strat_vars = "g", min_n = 0L)

  # drop_empty = FALSE is what lets the caller count and report the zero.
  raw <- gs_split_strata(dt, spec, drop_empty = FALSE)
  expect_named(raw, c("g_a", "g_b", "g_empty"))
  expect_equal(nrow(raw$g_empty), 0L)

  # By default the empty stratum never reaches the plotting code.
  expect_named(gs_split_strata(dt, spec), c("g_a", "g_b"))
})

test_that("gs_split_strata returns one unnamed element without stratification", {
  dt <- gs_prepare_data(iris)
  spec <- gs_spec(plot_type = "Boxplot", x = "Species", y = "Sepal.Length")
  out <- gs_split_strata(dt, spec)

  expect_length(out, 1L)
  expect_equal(names(out), "")
  expect_equal(nrow(out[[1]]), 150L)
})

test_that("gs_file_stem combines prefix and stratum name", {
  expect_equal(gs_file_stem("boxplot", "sex_M"), "boxplot_sex_M")
  expect_equal(gs_file_stem("", "sex_M"), "sex_M")
  expect_equal(gs_file_stem("boxplot", ""), "boxplot")
  expect_equal(gs_file_stem("", ""), "plot")
  expect_equal(gs_file_stem("fig", "arm: A/B"), "fig_arm_A_B")
})

test_that("gs_one_stratum subsets to the row of the strata table it is given", {
  dt <- gs_prepare_data(iris)
  spec <- gs_spec(plot_type = "Boxplot", y = "Sepal.Length",
                  strat_vars = "Species")
  st <- gs_strata_table(dt, "Species", 0L)

  out <- gs_one_stratum(dt, spec, st[level == "setosa"])
  expect_equal(nrow(out), 50L)
  expect_equal(unique(as.character(out$Species)), "setosa")

  # With no stratifying variables there is nothing to subset to.
  expect_equal(nrow(gs_one_stratum(dt, gs_spec(y = "Sepal.Length"), st[1])), 150L)
})

# --- crossed layers -----------------------------------------------------------

crossed_dt <- function() {
  data.table::data.table(
    sex = factor(rep(c("M", "F"), each = 30)),
    arm = factor(rep(c("A", "B", "C"), 20), levels = c("A", "B", "C", "D")),
    y = rnorm(60)
  )
}

test_that("crossed mode lists one stratum per observed combination", {
  st <- gs_strata_table(crossed_dt(), c("sex", "arm"), 0L, "crossed")

  # 2 x 3 observed; arm "D" recruited nobody, so 8 were possible.
  expect_equal(nrow(st), 6L)
  expect_equal(attr(st, "n_possible"), 8L)
  expect_equal(sum(st$n), 60L)
  expect_equal(st$var[1], "sex x arm")
  expect_equal(st$label[1], "sex: F | arm: A")
  expect_equal(st$file[1], "sex_F__arm_A")
  # Combinations are listed in level order -- F before M, because that is the
  # factor's own order -- with the outermost variable moving slowest.
  expect_equal(st$level, c("F | A", "F | B", "F | C", "M | A", "M | B", "M | C"))
})

test_that("crossed mode splits and previews the same combinations", {
  dt <- crossed_dt()
  spec <- gs_spec(plot_type = "Boxplot", y = "y",
                  strat_vars = c("sex", "arm"), strat_mode = "crossed",
                  min_n = 0L)

  out <- gs_split_strata(dt, spec)
  expect_length(out, 6L)
  expect_setequal(names(out), gs_strata_table(dt, c("sex", "arm"), 0L,
                                              "crossed")$file)
  expect_equal(sum(vapply(out, nrow, integer(1L))), 60L)

  # The all-figures preview labels each row with its one combination; nothing
  # is duplicated the way independent mode duplicates it.
  long <- gs_long_strata(dt, c("sex", "arm"), "y", 0L, "crossed")
  expect_equal(nrow(long), 60L)
  expect_equal(nlevels(long$.strat_label), 6L)
  expect_match(levels(long$.strat_label)[1], "sex: F | arm: A (N = 10)",
               fixed = TRUE)
})

# --- missing values -----------------------------------------------------------

test_that("a row that does not say which stratum it is in is excluded", {
  dt <- data.table::data.table(
    g = c(rep("a", 20), rep("b", 20), rep(NA_character_, 5)),
    # The one missing f is a row whose g is present, so it is an extra
    # exclusion rather than one already covered by g.
    f = c(NA_character_, rep(c("x", "y"), 22)),
    y = rnorm(45)
  )

  st <- gs_strata_table(dt, "g", 0L)
  expect_equal(nrow(st), 2L)
  expect_equal(sum(st$n), 40L)
  expect_false(any(is.na(st$level)))

  # gs_split_strata excludes the same rows, so no "g_NA" file is ever written.
  spec <- gs_spec(plot_type = "Boxplot", y = "y", strat_vars = "g", min_n = 0L)
  out <- gs_split_strata(dt, spec)
  expect_named(out, c("g_a", "g_b"))

  # The facet layer counts as a layer: its missing rows go too.
  spec2 <- gs_spec(plot_type = "Boxplot", y = "y", strat_vars = "g",
                   facet = "f", min_n = 0L)
  expect_equal(sum(vapply(gs_split_strata(dt, spec2), nrow, integer(1L))), 39L)
})

test_that("gs_missing_report counts only the variables that are missing", {
  dt <- data.table::data.table(a = c(1, NA, 3), b = c("x", "y", "z"),
                               c = c(NA, NA, 1))
  rep <- gs_missing_report(dt, c("a", "b", "c"))
  expect_equal(rep$var, c("a", "c"))
  expect_equal(rep$n_missing, c(1L, 2L))
  expect_equal(nrow(gs_missing_report(dt, "b")), 0L)
  expect_equal(nrow(gs_missing_report(dt, character())), 0L)
})

test_that("escaping a reserved name cannot collide with a real column", {
  # .strat_var is renamed to .strat_var_, which the data already has. Before
  # make.unique() ran last, both columns came out called .strat_var_ and the
  # app read the wrong one.
  d <- data.frame(a = 1:3, b = 4:6)
  names(d) <- c(".strat_var", ".strat_var_")
  out <- gs_prepare_data(d)
  expect_equal(anyDuplicated(names(out)), 0L)
  expect_equal(length(names(out)), 2L)

  # The same for a blank name that fills to one already in use.
  d2 <- data.frame(V1 = 1:3, x = 4:6)
  names(d2) <- c("V1", "")
  out2 <- gs_prepare_data(d2)
  expect_equal(anyDuplicated(names(out2)), 0L)
})

test_that("a list-column is refused at the door, not inside the app", {
  d <- data.frame(g = c("a", "b"), stringsAsFactors = FALSE)
  d$lst <- list(1:2, 3:4)
  expect_error(gs_prepare_data(d), "lst")
  expect_error(gs_prepare_data(d), "lists rather than values")
})

test_that("the shipped example data is what its documentation says it is", {
  expect_equal(dim(epi_cohort), c(600L, 11L))
  # The n = 0 stratum only works if Site D survives as a declared level.
  expect_true("Site D" %in% levels(epi_cohort$site))
  expect_equal(sum(epi_cohort$site == "Site D"), 0L)
  # The documented Severe count, and the fact that the default minimum keeps
  # it: the group is there to try the control on, not to be filtered by it.
  expect_equal(sum(epi_cohort$severity == "Severe"), 20L)
  expect_gt(sum(epi_cohort$severity == "Severe"), gs_spec()$min_n)
  # Follow-up is bounded by the one-year study end, and death is 0/1.
  expect_true(all(epi_cohort$fu_days >= 1 & epi_cohort$fu_days <= 365))
  expect_setequal(unique(epi_cohort$death), c(0L, 1L))
})
