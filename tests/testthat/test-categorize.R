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

test_that("each method produces the categorization it promises", {
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

test_that("categorizing never touches the data it was given", {
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

test_that("a default cut name never returns a name already taken", {
  taken <- c("x_cat", paste0("x_cat", 2:150))
  nm <- gs_cut_name("x", taken)
  expect_false(nm %in% taken)
})

test_that("the group-count bounds hold for a cut built without gs_cut()", {
  # gs_cut() floors n at 2, so the lower bound cannot be reached through it --
  # see the constructor test above. A cut assembled as a plain list skips that
  # flooring, and gs_check_cut() is what still catches it.
  dt <- data.table::data.table(age = as.numeric(1:100))
  raw <- list(var = "age", new = "age_cat", method = "quantile", n = 1L,
              breaks = numeric())
  expect_match(gs_check_cut(dt, raw), "between 2 and 20")
  expect_match(gs_check_cut(dt, gs_cut("age", n = 25)), "between 2 and 20")
  expect_equal(gs_check_cut(dt, gs_cut("age", n = 4)), character())
})

test_that("a cut that cannot be applied is reported, not thrown", {
  dt <- data.table::data.table(x = rep(1, 50))
  out <- gs_apply_cuts(dt, list(gs_cut("x", new = "x_cat", n = 4)))
  expect_true(length(attr(out, "gs_cut_error")) > 0L)
  expect_false("x_cat" %in% names(out))
})

# --- missing vs observed -----------------------------------------------------

# A cohort with the gaps a real one has: a lab value that was not always
# measured, and a severity grade that was not always recorded.
gappy <- function() {
  dt <- cohort()
  dt[seq_len(150L), bmi := NA_real_]
  dt[seq(2L, 160L, by = 2L), severity := NA]
  dt
}

test_that("a missingness rule names itself after the question it asks", {
  cut <- gs_cut("bmi", method = "missing")
  expect_equal(cut$new, "bmi_missing")
  expect_equal(gs_cut_name("bmi", character(), "missing"), "bmi_missing")
  expect_equal(gs_cut_name("bmi", "bmi_missing", "missing"), "bmi_missing2")
  expect_equal(gs_cut_describe(cut), "bmi_missing <- bmi (missing vs observed)")
})

test_that("missingness splits the rows in two and loses none of them", {
  dt <- gappy()
  out <- gs_apply_cuts(dt, list(gs_cut("bmi", method = "missing"),
                                gs_cut("severity", method = "missing")))

  expect_equal(levels(out$bmi_missing), c("Observed", "Missing"))
  expect_equal(as.integer(table(out$bmi_missing)),
               c(nrow(dt) - 150L, 150L))
  # The whole point: the indicator itself is never absent, so no row is
  # excluded at the layer stage for having no value for it.
  expect_false(anyNA(out$bmi_missing))
  expect_equal(nrow(gs_missing_report(out, c("bmi_missing", "severity_missing"))),
               0L)
})

test_that("missingness is a question a non-numeric column can answer too", {
  dt <- gappy()
  # A factor is refused by the methods that cut values and accepted by the one
  # that reads only whether there is a value.
  expect_match(gs_check_cut(dt, gs_cut("severity", method = "quantile")),
               "not numeric")
  expect_equal(gs_check_cut(dt, gs_cut("severity", method = "missing")),
               character())

  dt[, when := as.Date("2026-01-01") + seq_len(.N)]
  dt[1:10, when := NA]
  dt[, note := c(rep(NA_character_, 10L), rep("seen", .N - 10L))]
  expect_equal(gs_check_cut(dt, gs_cut("when", method = "missing")), character())
  expect_equal(gs_check_cut(dt, gs_cut("note", method = "missing")), character())
})

test_that("a variable with nothing missing is refused, and says why", {
  dt <- cohort()
  expect_match(gs_check_cut(dt, gs_cut("age", method = "missing")),
               "has no missing values")

  dt[, blank := NA_real_]
  expect_match(gs_check_cut(dt, gs_cut("blank", method = "missing")),
               "missing in every row")
})

test_that("a missingness variable can be stratified on like any other", {
  out <- gs_apply_cuts(gappy(), list(gs_cut("bmi", method = "missing")))
  info <- gs_classify_vars(out)
  expect_true(info[var == "bmi_missing", is_categorical])
  expect_true("bmi_missing" %in% gs_vars_of(info, "stratify"))
})

test_that("the variable pool widens for missingness and narrows back", {
  info <- gs_classify_vars(gs_apply_cuts(gappy(), list()))
  # severity is a factor: worth asking whether it is there, not worth cutting.
  expect_true("severity" %in% gs_selector_choices(info, "missing")$cut_var)
  expect_false("severity" %in% gs_selector_choices(info, "quantile")$cut_var)
  # age is continuous, so it is offered under both.
  expect_true("age" %in% gs_selector_choices(info, "missing")$cut_var)
  expect_true("age" %in% gs_selector_choices(info, "quantile")$cut_var)
  # The label follows the pool.
  expect_equal(gs_cut_var_label("missing"), "Variable")
  expect_equal(gs_cut_var_label("quantile"), "Continuous variable")
})

# --- time resolutions ---------------------------------------------------------

# Three years of dates, and the same three years carrying a time of day, so
# that every resolution has something to read and the two families can be told
# apart.
dated <- function() {
  days <- as.Date("2021-01-04") + seq(0, 1080, by = 9)
  data.table::data.table(
    d = days,
    id = data.table::as.IDate(days),
    # A time of day that varies but never runs past midnight, so `ts` and `d`
    # describe the same days and a resolution can be compared across the two.
    ts = as.POSIXct(paste(days, "06:20:00"), tz = "UTC") +
      (seq_along(days) %% 20L) * 1237,
    y = as.numeric(seq_along(days))
  )
}

derive <- function(dt, var, unit, ...) {
  rule <- gs_cut(var, method = "period", unit = unit, ...)
  out <- gs_apply_cuts(dt, list(rule))
  expect_null(attr(out, "gs_cut_error"))
  out[[rule$new]]
}

test_that("every time resolution reads the column it is given", {
  dt <- dated()
  # A calendar period stays a date, because a trend drawn against it has to be
  # spaced by elapsed time rather than by level number.
  for (u in GS_PERIOD_UNITS) {
    src <- if (u %in% GS_TIME_OF_DAY_UNITS) "ts" else "d"
    col <- derive(dt, src, u)
    expect_s3_class(col, if (u %in% GS_TIME_OF_DAY_UNITS) "POSIXct" else "Date")
    expect_false(is.factor(col), info = u)
  }
  # A position in the cycle is a factor, and its levels are declared in full
  # whether or not every one of them is used.
  for (u in GS_CYCLE_UNITS) {
    src <- if (u %in% GS_TIME_OF_DAY_UNITS) "ts" else "d"
    col <- derive(dt, src, u)
    expect_s3_class(col, "factor")
    expect_false(inherits(col, "ordered"), info = u)
  }
})

test_that("the months of the year run January to December, not alphabetically", {
  # The whole reason the levels are written out: sort() puts April first.
  col <- derive(dated(), "d", "month_of_year")
  expect_equal(levels(col), month.abb)
  expect_equal(gs_levels_of(col), month.abb)
})

test_that("the seasons keep their order from the month they are started at", {
  dt <- dated()
  month_of <- function(col, m) {
    unique(as.character(col[data.table::month(dt$d) == m]))
  }

  north <- derive(dt, "d", "season", season_start = 3L)
  expect_equal(levels(north), GS_SEASONS)
  expect_equal(month_of(north, 3L), "Spring")
  expect_equal(month_of(north, 7L), "Summer")
  expect_equal(month_of(north, 10L), "Fall")
  expect_equal(month_of(north, 12L), "Winter")

  # Starting in September moves the months between the seasons and leaves the
  # seasons themselves in the same order, which is what the control promises.
  south <- derive(dt, "d", "season", season_start = 9L)
  expect_equal(levels(south), GS_SEASONS)
  expect_equal(month_of(south, 9L), "Spring")
  expect_equal(month_of(south, 12L), "Summer")
})

test_that("the days of the week run Monday to Sunday, whatever numbers them", {
  # data.table's wday() counts Sunday as 1, so the lookup and the levels are
  # deliberately in different orders. Checked against strftime, which knows.
  dt <- dated()
  col <- derive(dt, "d", "day_of_week")
  expect_equal(levels(col),
               c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  expect_equal(as.character(col), format(dt$d, "%a"))
})

test_that("a calendar period sorts along real time rather than as text", {
  # A month floored to a date orders 2021-09 before 2021-10; the same months
  # written as text would not.
  col <- derive(dated(), "d", "month")
  expect_equal(gs_levels_of(col), sort(unique(as.character(col))))
  expect_true(all(data.table::mday(col) == 1L))
  expect_equal(min(col), as.Date("2021-01-01"))
})

test_that("an hour floored off a date-time keeps its instant and its zone", {
  # as.POSIXct(cut(x, "hour")) looks like the obvious way to do this and is
  # not: cut() writes its labels in the column's zone, as.POSIXct() reads them
  # back in the session's, and the instant moves by the offset between them.
  # Six hours, silently, for a UTC column read in US Central.
  dt <- data.table::data.table(
    ts = as.POSIXct(c("2024-01-15 08:37:12", "2024-06-15 13:59:59"), tz = "UTC")
  )
  col <- derive(dt, "ts", "hour")
  expect_equal(attr(col, "tzone"), "UTC")
  expect_equal(format(col, tz = "UTC"),
               c("2024-01-15 08:00:00", "2024-06-15 13:00:00"))

  mins <- derive(dt, "ts", "minute")
  expect_equal(format(mins, tz = "UTC"),
               c("2024-01-15 08:37:00", "2024-06-15 13:59:00"))
})

test_that("a date reads the same however it was typed", {
  # Requirement: base R, data.table and date-time columns describing the same
  # days must give the same answer, so that the method is about the data and
  # not about which package wrote it.
  dt <- dated()
  for (u in c("year", "quarter", "month", "week", "day", "month_of_year",
              "season", "quarter_of_year", "day_of_week")) {
    from_date <- derive(dt, "d", u)
    from_idate <- derive(dt, "id", u)
    from_posix <- derive(dt, "ts", u)
    expect_equal(as.character(from_idate), as.character(from_date), info = u)
    expect_equal(as.character(from_posix), as.character(from_date), info = u)
  }
})

test_that("a time resolution is refused when the column cannot answer it", {
  dt <- dated()
  dt[, txt := "not a date"]
  dt[, empty := as.Date(NA)]
  dt[, tod := data.table::as.ITime(ts)]

  problem <- function(...) gs_check_cut(dt, gs_cut(..., method = "period"))

  expect_match(problem("txt", unit = "month"), "not a date or a time")
  expect_match(problem("y", unit = "year"), "not a date or a time")
  expect_match(problem("empty", unit = "month"), "no values to read")
  # An hour off a plain date is 0 for every row: data.table answers rather
  # than refusing, so the generic "fewer than two groups" message would blame
  # the wrong thing.
  expect_match(problem("d", unit = "hour_of_day"), "no time of day")
  expect_match(problem("d", unit = "hour"), "no time of day")
  # And the mirror image, where data.table does refuse.
  expect_match(problem("tod", unit = "month"), "no date")
  expect_match(problem("tod", unit = "season"), "no date")

  # The combinations that do work are not refused.
  expect_length(problem("d", unit = "season"), 0L)
  expect_length(problem("ts", unit = "hour_of_day"), 0L)
  expect_length(problem("tod", unit = "hour_of_day"), 0L)
})

test_that("a cut keeps its resolution through gs_spec()", {
  # gs_spec() rebuilds every rule through gs_as_cuts(), which passes on only
  # the fields it knows about. A dropped unit would leave the app drawing one
  # thing and the generated script another.
  spec <- gs_spec(cuts = list(gs_cut("d", method = "period", unit = "season",
                                     season_start = 9L)))
  expect_equal(spec$cuts[[1L]]$unit, "season")
  expect_equal(spec$cuts[[1L]]$season_start, 9L)

  # And a rule handed in as a plain list, the way a saved spec would carry it.
  plain <- gs_spec(cuts = list(list(var = "d", method = "period",
                                    unit = "month_of_year")))
  expect_equal(plain$cuts[[1L]]$unit, "month_of_year")
  expect_equal(plain$cuts[[1L]]$new, "d_month")
})

test_that("a derived time variable is named and described by its resolution", {
  # Two resolutions of one column are two columns, so the name has to say
  # which is which rather than repeating the method.
  expect_equal(gs_cut("admit", method = "period", unit = "year")$new,
               "admit_year")
  expect_equal(gs_cut("admit", method = "period", unit = "season")$new,
               "admit_season")
  expect_equal(gs_cut("admit", method = "period", unit = "day_of_week")$new,
               "admit_weekday")
  # A collision between the two families is settled the way any other is.
  expect_equal(gs_cut_name("admit", "admit_month", "period", "month_of_year"),
               "admit_month2")

  expect_equal(gs_cut_describe(gs_cut("admit", method = "period",
                                      unit = "month_of_year")),
               "admit_month <- admit (by month of the year)")
  expect_match(gs_cut_describe(gs_cut("admit", method = "period",
                                      unit = "season", season_start = 9L)),
               "spring starting in September", fixed = TRUE)
  # Every resolution describes itself; none falls through to NULL, which would
  # take the sidebar list down with it.
  for (u in GS_TIME_UNIT_VALUES) {
    expect_type(gs_cut_describe(gs_cut("admit", method = "period", unit = u)),
                "character")
  }
})

test_that("an unusable resolution falls back rather than building a bad rule", {
  expect_equal(gs_cut("d", method = "period", unit = "fortnight")$unit, "month")
  expect_equal(gs_cut("d", method = "period", unit = "season",
                      season_start = 13L)$season_start, GS_SEASON_START)
  expect_equal(gs_cut("d", method = "period", unit = "season",
                      season_start = NA)$season_start, GS_SEASON_START)
})

test_that("the time-resolution line the app runs is the one it prints", {
  # The same contract the other methods are held to: the derived column in the
  # app is the column the generated script produces.
  dt <- dated()
  rule <- gs_cut("d", method = "period", unit = "season", season_start = 3L)
  applied <- gs_apply_cuts(dt, list(rule))

  env <- new.env(parent = asNamespace("data.table"))
  assign("dt", data.table::copy(dt), envir = env)
  eval(parse(text = gs_code_cut_line(rule, "dt")), envir = env)

  expect_equal(get("dt", envir = env)[[rule$new]], applied[[rule$new]])
})

test_that("a time resolution can be read by a later rule", {
  # The names accumulate as the list is walked, so a season derived from a
  # date is available to a rule added after it.
  dt <- dated()
  out <- gs_apply_cuts(dt, list(
    gs_cut("d", new = "season", method = "period", unit = "season"),
    gs_cut("season", new = "season_missing", method = "missing")
  ))
  expect_true(all(c("season", "season_missing") %in% names(out)))
  expect_equal(levels(out$season_missing), c("Observed", "Missing"))
})
