iris_spec <- function(...) {
  gs_spec(x = "Species", y = "Sepal.Length", strat_vars = "Species", ...)
}

new_tempdir <- function() {
  d <- file.path(tempdir(), paste0("ggstratify-", as.integer(runif(1, 1, 1e9))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

test_that("every plot type generates parseable, drawable code", {
  # iris plus the two columns a Kaplan-Meier curve needs.
  set.seed(1)
  dt <- gs_prepare_data(cbind(
    iris,
    fu = runif(nrow(iris), 1, 100),
    ev = rbinom(nrow(iris), 1, 0.6)
  ))

  for (type in GS_PLOT_TYPES) {
    spec <- gs_spec(
      plot_type = type, y = "Sepal.Length", strat_vars = "Species",
      binwidth = 0.2, time = "fu", event = "ev",
      # Scatter and Line put a variable on each axis, and a factor on the x
      # axis of a line plot is a line per point.
      x = if (type %in% GS_XY_TYPES) "Petal.Length" else "Species")
    code <- gs_code_figure(spec, "d")

    expect_silent(parse(text = paste(code, collapse = "\n")))

    p <- gs_eval_plot(code, dt)
    expect_s3_class(p, "ggplot")
    # ggplot is lazy; building is what actually exercises the layers.
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("each plot type maps to the expected geom", {
  expected <- c(
    "Boxplot"     = "geom_boxplot",
    "Density"     = "geom_density",
    "Dot + Error" = "stat_summary",
    "Dotplot"     = "geom_dotplot",
    "Histogram"   = "geom_histogram",
    "Scatter"     = "geom_point",
    "Violin"      = "geom_violin"
  )
  expected[[GS_KM]] <- "geom_step"
  expected[[GS_LINE]] <- "geom_line"
  # Every type the app offers is covered here, so a new one cannot be added
  # without saying what it draws.
  expect_setequal(names(expected), GS_PLOT_TYPES)

  for (type in names(expected)) {
    code <- paste(gs_code_plot(iris_spec(plot_type = type)), collapse = "\n")
    expect_match(code, expected[[type]], fixed = TRUE,
                 info = paste("plot type:", type))
  }
})

test_that("a line plot draws one line per ID, and the ID is a layer mapping", {
  # In the figure's own aes() the ID would be inherited by the smoother,
  # which would then be fitted per subject rather than through them.
  spec <- gs_spec(plot_type = GS_LINE, x = "visit", y = "crp", id = "id",
                  group = "treatment", smooth = TRUE)
  code <- paste(gs_code_plot(spec), collapse = "\n")

  expect_match(code, "geom_line(aes(group = id), alpha = 0.6)", fixed = TRUE)
  expect_match(code, "aes(x = visit, y = crp, colour = treatment)", fixed = TRUE)
  expect_false(grepl("aes(x = visit, y = crp, colour = treatment, group = id)",
                     code, fixed = TRUE))

  # No ID: the points are joined in x order, within each colour group.
  plain <- paste(gs_code_plot(gs_spec(plot_type = GS_LINE, x = "visit",
                                      y = "crp")), collapse = "\n")
  expect_match(plain, "geom_line(alpha = 0.6)", fixed = TRUE)
  expect_false(grepl("group =", plain, fixed = TRUE))
})

test_that("the observations are marked only when asked for", {
  with_points <- paste(gs_code_plot(gs_spec(plot_type = GS_LINE, x = "visit",
                                            y = "crp", line_points = TRUE)),
                       collapse = "\n")
  expect_match(with_points, "geom_point(size = 1, alpha = 0.6)", fixed = TRUE)

  without <- paste(gs_code_plot(gs_spec(plot_type = GS_LINE, x = "visit",
                                        y = "crp")), collapse = "\n")
  expect_false(grepl("geom_point", without, fixed = TRUE))
})

test_that("the LOWESS smoother is opt-in and names its method", {
  spec <- gs_spec(plot_type = "Scatter", x = "Petal.Length", y = "Sepal.Length")
  expect_false(grepl("geom_smooth", paste(gs_code_plot(spec), collapse = "\n"),
                     fixed = TRUE))

  smoothed <- gs_spec(plot_type = "Scatter", x = "Petal.Length",
                      y = "Sepal.Length", smooth = TRUE, smooth_span = 0.5)
  code <- gs_code_plot(smoothed)
  expect_match(paste(code, collapse = "\n"),
               'geom_smooth(method = "loess", formula = y ~ x, span = 0.5, se = FALSE)',
               fixed = TRUE)
  # The band is the other half of the option.
  expect_match(paste(gs_code_plot(gs_spec(plot_type = "Scatter", x = "Petal.Length",
                                          y = "Sepal.Length", smooth = TRUE,
                                          smooth_se = TRUE)), collapse = "\n"),
               "se = TRUE", fixed = TRUE)

  # And it draws: loess through 150 observations, one fit per group.
  dt <- gs_prepare_data(iris)
  grouped <- gs_spec(plot_type = "Scatter", x = "Petal.Length",
                     y = "Sepal.Length", group = "Species", smooth = TRUE)
  expect_no_error(ggplot2::ggplot_build(
    gs_eval_plot(gs_code_figure(grouped, "d"), dt)))
})

test_that("options belonging to one plot type never reach another", {
  # The controls are hidden when the plot type changes, but their last value
  # stays in the browser and would otherwise arrive in the next spec.
  spec <- gs_spec(plot_type = "Boxplot", y = "Sepal.Length", id = "id",
                  line_points = TRUE, smooth = TRUE)
  expect_equal(spec$id, "")
  expect_false(spec$line_points)
  expect_false(spec$smooth)

  code <- paste(gs_code_plot(spec), collapse = "\n")
  expect_false(grepl("geom_smooth", code, fixed = TRUE))
  expect_false(grepl("group = id", code, fixed = TRUE))

  # A line plot keeps all three, and the ID is read from the data.
  line <- gs_spec(plot_type = GS_LINE, x = "visit", y = "crp", id = "id",
                  line_points = TRUE, smooth = TRUE)
  expect_equal(line$id, "id")
  expect_true("id" %in% gs_spec_cols(line))
})

test_that("density and histogram take their variable from y and draw it on x", {
  spec <- gs_spec(plot_type = "Density", y = "Sepal.Length")
  expect_equal(spec$x, "Sepal.Length")
  expect_equal(spec$y, "")

  code <- paste(gs_code_plot(spec), collapse = "\n")
  expect_match(code, "aes(x = Sepal.Length)", fixed = TRUE)
})

test_that("jitter is honoured only where it applies", {
  box <- paste(gs_code_plot(iris_spec(plot_type = "Boxplot", jitter = TRUE)),
               collapse = "\n")
  expect_match(box, "position_jitter(width = 0.2, height = 0, seed = 1)",
               fixed = TRUE)
  expect_match(box, "geom_point", fixed = TRUE)
  # The outliers are hidden when the points themselves are drawn.
  expect_match(box, "outlier.shape = NA", fixed = TRUE)

  # Jitter is meaningless on a histogram and must not be emitted.
  hist <- paste(gs_code_plot(iris_spec(plot_type = "Histogram", jitter = TRUE)),
                collapse = "\n")
  expect_false(grepl("position_jitter", hist, fixed = TRUE))
})

test_that("the group aesthetic follows the plot type", {
  fill <- paste(gs_code_plot(iris_spec(plot_type = "Boxplot", group = "Species")),
                collapse = "\n")
  expect_match(fill, "fill = Species", fixed = TRUE)

  colour <- paste(gs_code_plot(gs_spec(plot_type = "Scatter", x = "Sepal.Width",
                                       y = "Sepal.Length", group = "Species")),
                  collapse = "\n")
  expect_match(colour, "colour = Species", fixed = TRUE)
})

test_that("a palette is emitted only when a grouping variable is set", {
  with_group <- paste(gs_code_plot(iris_spec(group = "Species", palette = "Set1")),
                      collapse = "\n")
  expect_match(with_group, "scale_fill_brewer(palette = \"Set1\")", fixed = TRUE)

  without <- paste(gs_code_plot(iris_spec(palette = "Set1")), collapse = "\n")
  expect_false(grepl("scale_fill_brewer", without, fixed = TRUE))
})

test_that("a continuous grouping variable takes the gradient palette scale", {
  # scale_fill_brewer() is discrete and would abort on a continuous variable.
  cont <- paste(gs_code_plot(iris_spec(group = "Petal.Width", palette = "Blues",
                                       group_continuous = TRUE)),
                collapse = "\n")
  expect_match(cont, "scale_fill_distiller(palette = \"Blues\")", fixed = TRUE)
  expect_false(grepl("scale_fill_brewer", cont, fixed = TRUE))

  disc <- paste(gs_code_plot(iris_spec(group = "Species", palette = "Blues")),
                collapse = "\n")
  expect_match(disc, "scale_fill_brewer(palette = \"Blues\")", fixed = TRUE)
})

test_that("an axis range is emitted only when one was typed", {
  none <- paste(gs_code_plot(iris_spec()), collapse = "\n")
  expect_false(grepl("coord_cartesian", none, fixed = TRUE))

  both <- paste(gs_code_plot(iris_spec(xlim_min = 0, xlim_max = 10,
                                       ylim_min = 4.5, ylim_max = 7.5)),
                collapse = "\n")
  expect_match(both, "coord_cartesian(xlim = c(0, 10), ylim = c(4.5, 7.5))",
               fixed = TRUE)

  # One axis, and one end of it, can be left to the data on its own.
  half <- paste(gs_code_plot(iris_spec(ylim_max = 7.5)), collapse = "\n")
  expect_match(half, "coord_cartesian(ylim = c(NA, 7.5))", fixed = TRUE)
  expect_false(grepl("xlim", half, fixed = TRUE))
})

test_that("a range zooms the figure rather than dropping the rows outside it", {
  # scale limits would filter first, which moves the median of what is drawn.
  # coord_cartesian() leaves the boxplot alone and looks at part of it.
  dt <- gs_prepare_data(iris)
  spec <- gs_spec(plot_type = "Boxplot", y = "Sepal.Length", ylim_min = 5,
                  ylim_max = 6)
  code <- gs_code_figure(spec, "d")

  expect_silent(parse(text = paste(code, collapse = "\n")))
  built <- ggplot2::ggplot_build(gs_eval_plot(code, dt))
  expect_equal(built$data[[1]]$middle, stats::median(iris$Sepal.Length))
})

test_that("facets come from the strata in facet mode and from the spec otherwise", {
  strat <- paste(gs_code_plot(iris_spec(), facet_strata = TRUE), collapse = "\n")
  expect_match(strat, "facet_wrap(~ .strat_label", fixed = TRUE)

  user <- paste(gs_code_plot(iris_spec(facet = "Species", show_n = FALSE)),
                collapse = "\n")
  expect_match(user, "facet_wrap(~ Species, labeller = label_both)", fixed = TRUE)

  none <- paste(gs_code_plot(iris_spec()), collapse = "\n")
  expect_false(grepl("facet_", none, fixed = TRUE))
})

test_that("each panel says how many observations it holds", {
  spec <- iris_spec(facet = "Species", show_n = TRUE)
  code <- gs_code_figure(spec, "d")
  flat <- paste(code, collapse = "\n")

  # The panel is drawn from a labelled copy of the column, not the column, so
  # that the strip can carry the size without touching the user's data.
  expect_match(flat, "add_facet_n <- function", fixed = TRUE)
  expect_match(flat, 'd <- add_facet_n(d, "Species")', fixed = TRUE)
  expect_match(flat, "facet_wrap(~ .facet_label)", fixed = TRUE)

  # Switching the sizes off goes back to the plain column, panelled through
  # label_both() so that the strip still says which variable it is a level of.
  plain <- paste(gs_code_figure(iris_spec(facet = "Species", show_n = FALSE), "d"),
                 collapse = "\n")
  expect_false(grepl("add_facet_n", plain, fixed = TRUE))
  expect_match(plain, "facet_wrap(~ Species, labeller = label_both)", fixed = TRUE)

  # And the labels are the variable and the sizes: setosa, versicolor and
  # virginica are 50 each.
  p <- gs_eval_plot(code, gs_prepare_data(iris))
  strips <- levels(p$data$.facet_label)
  expect_equal(strips, c("Species: setosa (N = 50)", "Species: versicolor (N = 50)",
                         "Species: virginica (N = 50)"))
})

test_that("a panel strip says which variable its level belongs to", {
  # A categorized variable's levels are bare ranges: a strip reading [4.3,5.4]
  # does not say what the range is a range of, and the axis and the legend are
  # describing other variables.
  dt <- gs_prepare_data(iris)
  cut <- gs_cut("Sepal.Length", new = "sl_cat", method = "breaks", breaks = 5.4)
  spec <- gs_spec(plot_type = "Boxplot", y = "Petal.Length", facet = "sl_cat",
                  cuts = list(cut))

  p <- gs_eval_plot(gs_code_figure(spec, "d"), gs_apply_cuts(dt, list(cut)))
  expect_true(all(grepl("^sl_cat: ", levels(p$data$.facet_label))))
})

test_that("a panel strip counts only the rows the figure draws", {
  # A stratum's panels are counted within the stratum, not across the data:
  # the figure and its strips have to agree about who is in it.
  dt <- gs_prepare_data(data.frame(
    g = rep(c("A", "B"), each = 30L),
    f = rep(c("x", "y", "x"), times = 20L),
    y = rnorm(60)
  ))
  spec <- gs_spec(plot_type = "Boxplot", y = "y", facet = "f",
                  strat_vars = "g", show_n = TRUE)
  p <- gs_eval_plot(gs_code_figure(spec, "d"), dt[g == "A"])

  expect_equal(sum(as.integer(sub(".*N = (\\d+).*", "\\1",
                                  levels(p$data$.facet_label)))), 30L)
})

test_that("boxplots survive having no x-variable", {
  spec <- gs_spec(plot_type = "Boxplot", y = "Sepal.Length")
  code <- gs_code_plot(spec)
  expect_match(paste(code, collapse = "\n"), 'aes(x = "", y = Sepal.Length)',
               fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(gs_eval_plot(code, gs_prepare_data(iris))))
})

test_that("awkward column names are backtick-quoted", {
  expect_equal(gs_bt("age"), "age")
  expect_equal(gs_bt("Sepal.Length"), "Sepal.Length")
  expect_equal(gs_bt("my var"), "`my var`")
  expect_equal(gs_bt("2010"), "`2010`")
  expect_equal(gs_bt("if"), "`if`")
  # Non-ASCII column names. Written as escapes so the source stays ASCII.
  jp <- "\u5e74\u9f62"
  expect_equal(gs_bt(jp), paste0("`", jp, "`"))

  spec <- gs_spec(plot_type = "Boxplot", x = "my group", y = jp)
  code <- paste(gs_code_plot(spec), collapse = "\n")
  expect_match(code, "x = `my group`", fixed = TRUE)
  expect_silent(parse(text = code))
})

test_that("labels are quoted safely", {
  spec <- iris_spec(title = 'He said "hi"', lab_x = "Species", lab_y = "Length")
  code <- paste(gs_code_plot(spec), collapse = "\n")
  expect_silent(parse(text = code))
  expect_match(code, "labs(", fixed = TRUE)
})

test_that("title_expr injects raw code rather than a string literal", {
  code <- paste(gs_code_plot(iris_spec(), title_expr = "nm"), collapse = "\n")
  expect_match(code, "labs(title = nm)", fixed = TRUE)
})

test_that("the generated code is one figure's worth, and nothing else", {
  spec <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bmi",
                  strat_vars = c("sex", "treatment"), min_n = 10L,
                  data_name = "mydata", outdir = "figures")
  script <- gs_code_script(spec)

  expect_silent(parse(text = script))
  # Writing the files is the Export button's job; the code tab is the figure.
  for (unwanted in c("iwalk", "ggsave", "dir.create", "outdir", "split(")) {
    expect_false(grepl(unwanted, script, fixed = TRUE), info = unwanted)
  }
  expect_match(script, "library(ggplot2)", fixed = TRUE)
  expect_match(script, "p <- ggplot(", fixed = TRUE)
})

test_that("the code subsets the data down to the figure being shown", {
  spec <- gs_spec(plot_type = "Boxplot", y = "bmi", strat_vars = "sex",
                  show_n = TRUE)
  row <- data.table::data.table(var = "sex", level = "Male", label = "sex: Male",
                                file = "sex_Male", n = 42L, keep = TRUE)
  script <- gs_code_script(spec, row)

  expect_silent(parse(text = script))
  expect_match(script, 'd <- dt[as.character(sex) == "Male"]', fixed = TRUE)
  expect_match(script, "# Figure: sex: Male (N = 42)", fixed = TRUE)
  expect_match(script, 'labs(title = "sex_Male (N = 42)")', fixed = TRUE)

  # Nothing to subset: the figure is drawn from every row. With nothing to
  # derive or exclude either, it is drawn from the object as it stands, and
  # the code takes no data.table dependency to say so.
  plain <- gs_code_script(gs_spec(plot_type = "Scatter", x = "a", y = "b",
                                  data_name = "mydata"))
  expect_silent(parse(text = plain))
  expect_match(plain, "d <- mydata", fixed = TRUE)
  expect_false(grepl("library(data.table)", plain, fixed = TRUE))
  expect_false(grepl("Figure:", plain, fixed = TRUE))

  # A layer brings data.table back, because the exclusion is written in it.
  layered <- gs_code_script(gs_spec(plot_type = "Scatter", x = "a", y = "b",
                                    facet = "g"))
  expect_match(layered, "library(data.table)", fixed = TRUE)
  expect_match(layered, "d <- dt", fixed = TRUE)
})

test_that("a crossed stratum is taken apart into one test per variable", {
  spec <- gs_spec(plot_type = "Boxplot", y = "bmi",
                  strat_vars = c("sex", "site"), strat_mode = "crossed")
  row <- data.table::data.table(var = "sex x site", level = "Male | Site A",
                                label = "sex: Male | site: Site A",
                                file = "sex_Male__site_Site_A", n = 30L,
                                keep = TRUE)
  script <- gs_code_script(spec, row)

  expect_silent(parse(text = script))
  expect_match(script,
               paste0('d <- dt[as.character(sex) == "Male" & ',
                      'as.character(site) == "Site A"]'),
               fixed = TRUE)
})

test_that("a stratum that cannot be taken apart yields no subset at all", {
  # A level containing the separator itself would otherwise be split into the
  # wrong number of pieces, and a wrong subset is worse than none.
  spec <- gs_spec(plot_type = "Boxplot", y = "bmi",
                  strat_vars = c("sex", "site"), strat_mode = "crossed")
  row <- data.table::data.table(var = "sex x site", level = "Male",
                                label = "x", file = "x", n = 1L, keep = TRUE)
  expect_length(gs_stratum_levels(row, c("sex", "site"), "crossed"), 0L)
  expect_match(gs_code_script(spec, row), "d <- dt", fixed = TRUE)
})

test_that("the stratum size is written into the figure title", {
  row <- data.table::data.table(var = "sex", level = "M", label = "sex: M",
                                file = "sex_M", n = 42L, keep = TRUE)
  with_n <- gs_code_script(gs_spec(plot_type = "Boxplot", y = "bmi",
                                   strat_vars = "sex", show_n = TRUE), row)
  expect_match(with_n, 'labs(title = "sex_M (N = 42)")', fixed = TRUE)
  expect_silent(parse(text = with_n))

  without <- gs_code_script(gs_spec(plot_type = "Boxplot", y = "bmi",
                                    strat_vars = "sex", show_n = FALSE), row)
  expect_match(without, 'labs(title = "sex_M")', fixed = TRUE)
  expect_silent(parse(text = without))
})

test_that("a title the user typed replaces the stratum name", {
  # "blank = stratum name" is what the control promises, so a typed title has
  # to win.
  spec <- gs_spec(plot_type = "Boxplot", y = "bmi", strat_vars = "sex",
                  title = "Length of stay", show_n = TRUE)
  expect_equal(gs_title_literal(spec, "sex_M", 42L),
               "\"Length of stay (N = 42)\"")
  expect_equal(gs_title_literal(gs_spec(title = "", show_n = TRUE), "sex_M", 42L),
               "\"sex_M (N = 42)\"")
  expect_equal(gs_title_literal(gs_spec(title = "Fixed", show_n = FALSE),
                                "sex_M", 42L),
               "\"Fixed\"")
  expect_null(gs_title_literal(gs_spec(), "", NULL))
})

test_that("the generated code draws the figure it says it draws", {
  dir <- new_tempdir()
  spec <- gs_spec(plot_type = "Boxplot", y = "Sepal.Length", facet = "Species",
                  data_name = "iris")
  env <- new.env(parent = globalenv())
  p <- eval(parse(text = gs_code_script(spec)), envir = env)

  expect_s3_class(p, "ggplot")
  ggplot2::ggsave(file.path(dir, "p.png"), p, width = 5, height = 4,
                  device = ragg::agg_png)
  expect_gt(file.size(file.path(dir, "p.png")), 0)
})

test_that("each format is written through a device that can write it", {
  expect_identical(gs_device("png"), ragg::agg_png)
  # svglite when it is there, and the cairo device R ships with when it is not,
  # so that the option works without a further install either way.
  expect_true(is.function(gs_device("svg")))

  dir <- new_tempdir()
  p <- gs_eval_plot(gs_code_figure(gs_spec(plot_type = "Boxplot",
                                           y = "Sepal.Length"), "d"),
                    gs_prepare_data(iris))
  for (fmt in GS_FORMATS) {
    f <- file.path(dir, paste0("p.", fmt))
    ggplot2::ggsave(f, p, width = 5, height = 4, device = gs_device(fmt))
    expect_gt(file.size(f), 0)
  }
  # An SVG is text, and says so in its first line.
  expect_match(readLines(file.path(dir, "p.svg"), n = 2L, warn = FALSE)[1],
               "xml|svg")
})

test_that("an unknown format falls back to PNG rather than to nothing", {
  expect_equal(gs_spec(format = "tiff")$format, "png")
  expect_equal(gs_spec(format = "svg")$format, "svg")
})

test_that("gs_default_prefix produces file-safe stems", {
  expect_equal(gs_default_prefix("Boxplot"), "boxplot")
  expect_equal(gs_default_prefix("Dot + Error"), "dot_error")
})

test_that("gs_n formats numbers without scientific notation", {
  expect_equal(gs_n(300), "300")
  expect_equal(gs_n(0.6), "0.6")
  expect_equal(gs_n(1e6), "1000000")
  expect_equal(gs_n(NA), "NA")
})

test_that("the generated script derives a missingness variable the same way", {
  # The single implementation rule: what the app holds is what the printed
  # script produces, because the app runs the very line it prints.
  dt <- gs_prepare_data(iris)
  dt[1:20, Sepal.Length := NA_real_]
  cut <- gs_cut("Sepal.Length", method = "missing")

  line <- gs_code_cut_line(cut, "dt")
  expect_equal(
    line,
    paste0("dt[, Sepal.Length_missing := factor(is.na(Sepal.Length), ",
           "levels = c(FALSE, TRUE), labels = c(\"Observed\", \"Missing\"))]"))

  in_app <- gs_apply_cuts(dt, list(cut))$Sepal.Length_missing
  from_script <- local({
    d <- data.table::copy(dt)
    eval(parse(text = gs_code_cut_line(cut, "d")))
    d$Sepal.Length_missing
  })
  expect_equal(in_app, from_script)
  expect_equal(as.integer(table(in_app)), c(130L, 20L))
})

test_that("a missingness variable keeps every row through the layer stage", {
  # The block that excludes rows with no value for a layer variable is emitted
  # whatever the layer is; a missingness indicator simply gives it nothing to
  # exclude, which is what makes the two groups add up to the whole data.
  dt <- gs_prepare_data(iris)
  dt[1:20, Sepal.Length := NA_real_]
  cut <- gs_cut("Sepal.Length", method = "missing")
  spec <- gs_spec(plot_type = "Boxplot", y = "Petal.Length",
                  facet = "Sepal.Length_missing", cuts = list(cut))

  p <- gs_eval_plot(gs_code_figure(spec, "d"),
                    gs_apply_cuts(dt, list(cut)))
  expect_equal(nrow(p$data), nrow(dt))
  expect_equal(nlevels(droplevels(p$data$.facet_label)), 2L)
})

# --- the date axis ------------------------------------------------------------

line_spec <- function(...) {
  gs_spec(plot_type = GS_LINE, x = "admit", y = "crp", ...)
}

dated_line <- function() {
  days <- as.Date("2021-01-15") + seq(0, 1080, by = 15)
  data.table::data.table(admit = days, crp = as.numeric(seq_along(days)))
}

test_that("a month of the year is written the way it would be written by hand", {
  # The generated line is the idiom an analyst would type, not a paraphrase of
  # it: month.abb is base R and month() is data.table's, both already attached
  # by the script that runs this.
  rule <- gs_cut("date", new = "month", method = "period",
                 unit = "month_of_year")
  expect_equal(
    gs_code_cut_line(rule, "dt"),
    "dt[, month := factor(month.abb[month(date)], levels = month.abb)]")
})

test_that("a line plot ticks its date axis at the spacing asked for", {
  spec <- line_spec(x_time_class = "date", x_time_unit = "year",
                    x_time_labels = "%Y")
  expect_equal(gs_code_time_scale(spec),
               "scale_x_date(date_breaks = \"1 year\", date_labels = \"%Y\") +")

  # Either half stands on its own: a spacing with ggplot2's own tick text, or
  # ggplot2's own spacing relabelled.
  expect_equal(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_unit = "month")),
               "scale_x_date(date_breaks = \"1 month\") +")
  expect_equal(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_labels = "%b %Y")),
               "scale_x_date(date_labels = \"%b %Y\") +")
  # And neither is nothing at all.
  expect_length(gs_code_time_scale(line_spec(x_time_class = "date")), 0L)
})

test_that("a quarterly axis is ticked every three months, because ggplot2 has no quarter", {
  # "1 quarter" is not a unit seq.Date knows: it aborts the figure with
  # "'from' must be a finite number", which says nothing about what was wrong.
  expect_equal(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_unit = "quarter")),
               "scale_x_date(date_breaks = \"3 months\") +")
  # A count multiplies the unit rather than being pasted in front of it.
  expect_equal(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_unit = "quarter",
                                            x_time_every = 2)),
               "scale_x_date(date_breaks = \"6 months\") +")
  expect_equal(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_unit = "year",
                                            x_time_every = 5)),
               "scale_x_date(date_breaks = \"5 years\") +")
})

test_that("a date-time axis gets the scale that can draw it", {
  # The wrong one of the two does not relabel the axis, it refuses to draw it.
  expect_match(gs_code_time_scale(line_spec(x_time_class = "datetime",
                                            x_time_unit = "hour")),
               "^scale_x_datetime\\(")
  expect_match(gs_code_time_scale(line_spec(x_time_class = "date",
                                            x_time_unit = "day")),
               "^scale_x_date\\(")
})

test_that("an hourly tick spacing is dropped from an axis of plain dates", {
  # scale_x_date() does not ignore it -- it stops with "invalid specification
  # of 'breaks'" and no figure appears.
  expect_equal(line_spec(x_time_class = "date", x_time_unit = "hour")$x_time_unit,
               "")
  expect_equal(line_spec(x_time_class = "date", x_time_unit = "minute")$x_time_unit,
               "")
  # The same units are fine over a column that carries a time of day.
  expect_equal(line_spec(x_time_class = "datetime",
                         x_time_unit = "hour")$x_time_unit, "hour")
})

test_that("the date axis belongs to the line plot alone", {
  for (type in setdiff(GS_PLOT_TYPES, GS_LINE)) {
    spec <- gs_spec(plot_type = type, x = "admit", y = "crp",
                    x_time_class = "date", x_time_unit = "year")
    expect_equal(spec$x_time_unit, "", info = type)
    expect_length(gs_code_time_scale(spec), 0L)
  }
  # And to one whose X really is a date.
  expect_equal(line_spec(x_time_unit = "year")$x_time_unit, "")
})

test_that("a numeric axis range is dropped on a date axis rather than breaking the figure", {
  # ggplot2 does not ignore a number handed to a date scale: it stops with
  # "transform_date() works with objects of class <Date> only" and draws
  # nothing. The X boxes are numeric, so this is reachable by typing.
  spec <- line_spec(x_time_class = "date", xlim_min = 1, xlim_max = 9,
                    ylim_min = 0, ylim_max = 50)
  coord <- gs_code_coord(spec)
  expect_false(grepl("xlim", coord, fixed = TRUE))
  expect_match(coord, "ylim = c(0, 50)", fixed = TRUE)

  # A numeric axis keeps the range it was given.
  expect_match(gs_code_coord(line_spec(xlim_min = 1, xlim_max = 9)),
               "xlim = c(1, 9)", fixed = TRUE)

  # End to end: the figure draws instead of aborting.
  code <- gs_code_plot(spec, "d")
  expect_s3_class(gs_eval_plot(code, dated_line()), "ggplot")
})

test_that("a date axis is titled by its unit unless the user named it", {
  # The column plotted is still a column of dates; what the ticks read is what
  # the axis is now about.
  expect_match(gs_code_labs(line_spec(x_time_class = "date",
                                      x_time_unit = "year"), "colour", FALSE),
               "x = \"Year\"", fixed = TRUE)
  expect_match(gs_code_labs(line_spec(x_time_class = "date",
                                      x_time_unit = "month"), "colour", FALSE),
               "x = \"Month\"", fixed = TRUE)
  # A typed label still wins, and no unit means no title at all.
  expect_match(gs_code_labs(line_spec(x_time_class = "date",
                                      x_time_unit = "year",
                                      lab_x = "Admission"), "colour", FALSE),
               "x = \"Admission\"", fixed = TRUE)
  expect_length(gs_code_labs(line_spec(x_time_class = "date"), "colour", FALSE),
                0L)
})

test_that("a date axis draws, ticked and labelled as it was asked for", {
  spec <- line_spec(x_time_class = "date", x_time_unit = "year",
                    x_time_labels = "%Y")
  code <- gs_code_plot(spec, "d")
  p <- gs_eval_plot(code, dated_line())
  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  labels <- built$layout$panel_params[[1L]]$x$get_labels()
  labels <- labels[!is.na(labels)]
  # Three calendar years of data, ticked once a year and read as bare years.
  expect_true(all(grepl("^[0-9]{4}$", labels)))
  expect_true(all(c("2021", "2022", "2023") %in% labels))
  expect_equal(p$labels$x, "Year")
})

test_that("the generated code runs for every time resolution", {
  # Each resolution goes into the data through the same line the script prints,
  # and the figure drawn from the result is a real one.
  days <- as.Date("2021-01-04") + seq(0, 1080, by = 21)
  dt <- data.table::data.table(
    d = days,
    ts = as.POSIXct(paste(days, "06:20:00"), tz = "UTC") +
      (seq_along(days) %% 20L) * 1237,
    y = as.numeric(seq_along(days))
  )
  for (u in GS_TIME_UNIT_VALUES) {
    src <- if (u %in% GS_TIME_OF_DAY_UNITS) "ts" else "d"
    rule <- gs_cut(src, method = "period", unit = u)
    out <- gs_apply_cuts(dt, list(rule))
    expect_null(attr(out, "gs_cut_error"), info = u)

    spec <- gs_spec(plot_type = "Boxplot", x = rule$new, y = "y")
    code <- gs_code_plot(spec, "d")
    expect_s3_class(gs_eval_plot(code, out), "ggplot")
  }
})

# --- what the error bar stands for --------------------------------------------

err_data <- function() {
  set.seed(7)
  d <- data.table::data.table(
    arm = factor(rep(c("Control", "Treated"), each = 60)),
    bp = c(stats::rnorm(60, 140, 12), stats::rnorm(60, 132, 12)),
    died = c(stats::rbinom(60, 1, 0.30), stats::rbinom(60, 1, 0.12))
  )
  d
}

err_bars <- function(d, err_type, y, level = 0.95) {
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = y,
                  err_type = err_type, err_level = level)
  code <- c(gs_code_preamble(spec), gs_code_plot(spec, "d"))
  ggplot2::ggplot_build(gs_eval_plot(code, d))$data[[1L]]
}

test_that("the mean interval is the one t.test would give", {
  # Written out rather than taken from Hmisc's smean.cl.normal, so the claim
  # that it is the same interval is worth checking rather than asserting.
  d <- err_data()
  bars <- err_bars(d, "normal", "bp")
  ref <- vapply(split(d$bp, d$arm), function(v) stats::t.test(v)$conf.int,
                numeric(2L))
  expect_equal(bars$ymin, unname(ref[1L, ]))
  expect_equal(bars$ymax, unname(ref[2L, ]))

  # And it is a confidence interval, which is to say wider than one standard
  # error either side -- the difference the control exists to let you state.
  se <- err_bars(d, "se", "bp")
  expect_true(all(bars$ymax - bars$ymin > se$ymax - se$ymin))
  expect_equal(bars$y, se$y)
})

test_that("the confidence level is the one that was asked for", {
  d <- err_data()
  ninety <- err_bars(d, "normal", "bp", 0.90)
  ninety_nine <- err_bars(d, "normal", "bp", 0.99)
  expect_true(all(ninety_nine$ymax - ninety_nine$ymin >
                    ninety$ymax - ninety$ymin))
  ref <- vapply(split(d$bp, d$arm),
                function(v) stats::t.test(v, conf.level = 0.99)$conf.int,
                numeric(2L))
  expect_equal(ninety_nine$ymin, unname(ref[1L, ]))
})

test_that("the exact proportion interval is Clopper-Pearson", {
  d <- err_data()
  bars <- err_bars(d, "exact", "died")
  ref <- vapply(split(d$died, d$arm),
                function(v) stats::binom.test(sum(v), length(v))$conf.int,
                numeric(2L))
  expect_equal(bars$ymin, unname(ref[1L, ]))
  expect_equal(bars$ymax, unname(ref[2L, ]))
  expect_equal(bars$y, unname(vapply(split(d$died, d$arm), mean, numeric(1L))))
})

test_that("the Wilson interval is the score interval, not a Wald one", {
  # prop.test(correct = FALSE) is the same interval, so it is the reference.
  d <- err_data()
  bars <- err_bars(d, "wilson", "died")
  ref <- vapply(split(d$died, d$arm), function(v)
    suppressWarnings(stats::prop.test(sum(v), length(v),
                                      correct = FALSE)$conf.int),
    numeric(2L))
  expect_equal(bars$ymin, unname(ref[1L, ]), tolerance = 1e-8)
  expect_equal(bars$ymax, unname(ref[2L, ]), tolerance = 1e-8)
})

test_that("a proportion interval stays inside 0 and 1 and never collapses", {
  # The reason the two proportion methods are here at all. A mean plus or minus
  # a standard error on a group where nobody had the outcome has zero width and
  # claims certainty; both of these keep a real upper end.
  none <- data.table::data.table(arm = factor(rep("A", 25L)),
                                 died = rep(0L, 25L))
  all_of_them <- data.table::data.table(arm = factor(rep("A", 25L)),
                                        died = rep(1L, 25L))
  for (type in GS_ERR_PROP_TYPES) {
    empty <- err_bars(none, type, "died")
    expect_equal(empty$y, 0)
    expect_equal(empty$ymin, 0)
    expect_gt(empty$ymax, 0)
    expect_lte(empty$ymax, 1)

    full <- err_bars(all_of_them, type, "died")
    expect_equal(full$y, 1)
    expect_equal(full$ymax, 1)
    expect_lt(full$ymin, 1)
    expect_gte(full$ymin, 0)
  }
})

test_that("a single observation gets a point and no interval", {
  # qt() at zero degrees of freedom is NaN and sd() of one value is NA, so this
  # is guarded rather than left to produce a bar of unknown length.
  one <- data.table::data.table(arm = factor("A"), bp = 140)
  bars <- err_bars(one, "normal", "bp")
  expect_equal(bars$y, 140)
  expect_true(is.na(bars$ymin))
  expect_true(is.na(bars$ymax))
})

test_that("the standard error is still what a Dot + Error draws by default", {
  # The setting is new; the figure it makes when nothing is chosen is not.
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp")
  expect_equal(spec$err_type, "se")
  expect_equal(gs_code_dot_geom(spec),
               "stat_summary(fun.data = mean_se, geom = \"pointrange\")")
  # ggplot2 supplies mean_se, so the default needs nothing defined above it.
  expect_length(gs_code_preamble(spec), 0L)
})

test_that("each interval brings the helper that computes it, and only it", {
  for (type in setdiff(GS_ERR_TYPES, "se")) {
    spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", err_type = type)
    preamble <- gs_code_preamble(spec)
    expect_gt(length(preamble), 0L)
    # The layer names the function, and the function is defined above it.
    fun <- sub("^stat_summary\\(fun.data = ([^,]+),.*$", "\\1",
               gs_code_dot_geom(spec))
    expect_true(any(grepl(paste0("^", fun, " <- function"), preamble)),
                info = type)
    expect_match(gs_code_dot_geom(spec), "fun.args = list(conf = 0.95)",
                 fixed = TRUE)
  }
})

test_that("the error bar setting cannot reach another plot type's code", {
  # As with every other option that belongs to one type: a setting left behind
  # would otherwise call a helper that is no longer being defined.
  for (type in setdiff(GS_PLOT_TYPES, GS_DOT)) {
    spec <- gs_spec(plot_type = type, x = "arm", y = "bp", err_type = "wilson",
                    dot_line = TRUE)
    expect_equal(spec$err_type, "se", info = type)
    expect_false(spec$dot_line, info = type)
  }
})

test_that("the dots can be joined by a line, and are not by default", {
  # The setting is new; the figure drawn when nothing is asked for is not.
  plain <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp")
  expect_false(plain$dot_line)
  expect_length(gs_code_dot_geom(plain), 1L)

  joined <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", dot_line = TRUE)
  layers <- gs_code_dot_geom(joined)
  expect_length(layers, 2L)
  # Under the points rather than across them.
  expect_match(layers[1L], 'geom = "line"', fixed = TRUE)
  expect_match(layers[2L], 'geom = "pointrange"', fixed = TRUE)
  # A categorical x axis is one ggplot2 group per point, so a line that did
  # not say what it joins would join nothing.
  expect_match(layers[1L], "aes(group = 1)", fixed = TRUE)

  # And the line is where the dots are.
  built <- ggplot2::ggplot_build(gs_eval_plot(
    c(gs_code_preamble(joined), gs_code_plot(joined, "d")), err_data()))$data
  expect_equal(built[[1L]]$y, built[[2L]]$y)
})

test_that("the line is computed by the summary whose points it joins", {
  # Not merely something that looks like it: the same helper at the same
  # confidence level, so that the line passes through the dots by
  # construction. geom_line() reads x and y and ignores the interval.
  for (type in GS_ERR_TYPES) {
    y <- if (type %in% GS_ERR_PROP_TYPES) "died" else "bp"
    spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = y, err_type = type,
                    err_level = 0.9,
                    err_event = if (type %in% GS_ERR_PROP_TYPES) "1" else "")
    spec$dot_line <- TRUE
    layers <- gs_code_dot_geom(spec)
    expect_true(all(grepl(gs_dot_stat_args(spec), layers, fixed = TRUE)),
                info = type)

    built <- ggplot2::ggplot_build(gs_eval_plot(
      c(gs_code_preamble(spec), gs_code_plot(spec, "d")), err_data()))$data
    expect_equal(built[[1L]]$y, built[[2L]]$y, info = type)
  }
})

test_that("a grouped figure gets one line per colour", {
  # Which is the comparison the colours were asking for; one line through
  # every point of every group would be a different figure.
  grouped <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", group = "sex",
                     dot_line = TRUE)
  expect_match(gs_code_dot_geom(grouped)[1L], "aes(group = sex)", fixed = TRUE)
})

test_that("a proportion interval is refused for a measurement, and the reverse", {
  d <- err_data()
  info <- gs_classify_vars(d)
  expect_true("died" %in% gs_vars_of(info, "binary"))
  expect_false("bp" %in% gs_vars_of(info, "binary"))

  wrong_way <- gs_validate_spec(
    gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", err_type = "wilson"), info)
  expect_match(wrong_way, "exactly two values")

  # And a 0/1 outcome described by a mean and a standard error is told what to
  # use instead, rather than only that it is not continuous.
  other_way <- gs_validate_spec(
    gs_spec(plot_type = GS_DOT, x = "arm", y = "died", err_type = "se"), info)
  expect_match(other_way, "proportion intervals")

  for (type in GS_ERR_PROP_TYPES) {
    expect_length(gs_validate_spec(
      gs_spec(plot_type = GS_DOT, x = "arm", y = "died", err_type = type),
      info), 0L)
  }
})

test_that("an outcome is recognised however it was written", {
  # 0/1, TRUE/FALSE, a two-level factor and a two-valued character column are
  # all ordinary ways to keep a binary outcome, and all four are accepted.
  expect_equal(gs_binary_values(c(0L, 1L, 1L)), c("0", "1"))
  expect_equal(gs_binary_values(c(TRUE, FALSE, NA)), c("FALSE", "TRUE"))
  expect_equal(gs_binary_values(factor(c("No", "Yes"))), c("No", "Yes"))
  expect_equal(gs_binary_values(c("No", "Yes", "Yes")), c("No", "Yes"))
  for (col in list(c(0L, 1L), c(TRUE, FALSE), factor(c("No", "Yes")),
                   c("No", "Yes"))) {
    expect_true(gs_is_binary_col(col))
  }

  # A declared level nobody had is still one of the two, and so is the unseen
  # half of a 0/1 column: a stratum where the outcome never happened is a
  # proportion of 0, which is the case these intervals are most useful for.
  expect_equal(gs_binary_values(factor(c("No", "No"), levels = c("No", "Yes"))),
               c("No", "Yes"))
  expect_equal(gs_binary_values(rep(0L, 5L)), c("0", "1"))

  # Three values is not an outcome, and neither is a measurement or a date.
  expect_false(gs_is_binary_col(factor(c("a", "b", "c"))))
  expect_false(gs_is_binary_col(c(1.5, 0)))
  expect_false(gs_is_binary_col(as.Date(c("2021-01-01", "2021-01-02"))))
  # 1/2 is an event coding, which Surv() reads, but it does not say which of
  # the two values a proportion would be counting.
  expect_true(gs_is_event_col(c(1L, 2L)))
  expect_false(gs_is_binary_col(c(1L, 2L)))
})

test_that("the second value is counted unless another one is chosen", {
  # R's own convention -- the reference level first, the level being modelled
  # second -- but a default rather than a rule, because a factor declared
  # c("Yes", "No") means the opposite of one declared the other way round.
  d <- data.table::data.table(
    num = c(0L, 1L),
    lgl = c(FALSE, TRUE),
    fct = factor(c("No", "Yes"), levels = c("No", "Yes")),
    rev = factor(c("No", "Yes"), levels = c("Yes", "No")),
    bp = c(1.5, 2.5)
  )
  expect_equal(gs_err_event(d, "num"), "1")
  expect_equal(gs_err_event(d, "lgl"), "TRUE")
  expect_equal(gs_err_event(d, "fct"), "Yes")
  expect_equal(gs_err_event(d, "rev"), "No")
  expect_equal(gs_err_event(d, "fct", "No"), "No")

  # A choice left behind by a different Y variable is not carried over, and a
  # column that is not an outcome has nothing to count.
  expect_equal(gs_err_event(d, "num", "Yes"), "1")
  expect_equal(gs_err_event(d, "bp"), "")
  expect_equal(gs_err_event(d, ""), "")
  expect_equal(gs_err_event(d, "gone"), "")
})

test_that("a proportion counts a value in the figure rather than recoding first", {
  # Written into the aes() so that an outcome kept as a factor is drawn as it
  # is, and so that the figure says which of the two values it counted.
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "outcome",
                  err_type = "wilson", err_event = "Yes")
  expect_equal(gs_y_expr(spec), "as.integer(outcome == \"Yes\")")
  expect_match(gs_code_labs(spec, "colour", FALSE),
               'y = "Proportion outcome = Yes"', fixed = TRUE)

  # Every other figure reads the column itself.
  expect_equal(gs_y_expr(gs_spec(plot_type = "Boxplot", y = "bp")), "bp")
  expect_equal(gs_y_expr(gs_spec(plot_type = GS_DOT, x = "arm", y = "bp",
                                 err_type = "normal")), "bp")
  # And a counted value cannot survive a switch back to a mean.
  expect_equal(gs_spec(plot_type = GS_DOT, y = "bp", err_type = "se",
                       err_event = "Yes")$err_event, "")
})

test_that("the same outcome gives the same interval however it was written", {
  set.seed(11)
  ev <- stats::rbinom(60L, 1L, 0.35)
  d <- data.table::data.table(
    arm = factor(rep(c("A", "B"), each = 30L)),
    num = ev,
    lgl = as.logical(ev),
    fct = factor(ifelse(ev == 1L, "Yes", "No"), levels = c("No", "Yes")),
    chr = ifelse(ev == 1L, "Yes", "No")
  )
  ref <- vapply(split(ev, d$arm), function(x)
    suppressWarnings(stats::prop.test(sum(x), length(x),
                                      correct = FALSE)$conf.int),
    numeric(2L))

  for (v in c("num", "lgl", "fct", "chr")) {
    spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = v, err_type = "wilson",
                    err_event = gs_err_event(d, v))
    code <- c(gs_code_preamble(spec), gs_code_plot(spec, "d"))
    bars <- ggplot2::ggplot_build(gs_eval_plot(code, d))$data[[1L]]
    expect_equal(bars$ymin, unname(ref[1L, ]), tolerance = 1e-8)
    expect_equal(bars$ymax, unname(ref[2L, ]), tolerance = 1e-8)
  }
})

test_that("counting the other value gives the other proportion", {
  d <- data.table::data.table(
    arm = factor(rep("A", 20L)),
    fct = factor(rep(c("No", "Yes"), c(15L, 5L)), levels = c("No", "Yes"))
  )
  p_of <- function(event) {
    spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "fct",
                    err_type = "wilson", err_event = event)
    ggplot2::ggplot_build(gs_eval_plot(
      c(gs_code_preamble(spec), gs_code_plot(spec, "d")), d))$data[[1L]]$y
  }
  expect_equal(p_of("Yes"), 0.25)
  expect_equal(p_of("No"), 0.75)
})

test_that("an unusable error bar or level falls back rather than breaking", {
  expect_equal(gs_spec(plot_type = GS_DOT, y = "bp",
                       err_type = "bootstrap")$err_type, "se")
  for (bad in list(NA, 0, 1, 1.5, "wide")) {
    expect_equal(gs_spec(plot_type = GS_DOT, y = "bp", err_type = "normal",
                         err_level = bad)$err_level, 0.95)
  }
  expect_equal(gs_spec(plot_type = GS_DOT, y = "bp", err_type = "normal",
                       err_level = 0.9)$err_level, 0.9)
})

# --- turning the tick labels --------------------------------------------------

test_that("a tick angle is emitted after the theme, or not at all", {
  flat <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bp")
  expect_length(gs_code_tick_angle(flat), 0L)

  turned <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bp",
                    tick_angle_x = 45)
  expect_equal(gs_code_tick_angle(turned),
               "theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))")
  # Upright text is centred on its tick rather than pulled off the end of it.
  expect_match(gs_code_tick_angle(gs_spec(tick_angle_x = 90)),
               "angle = 90, hjust = 1, vjust = 0.5", fixed = TRUE)
  expect_match(gs_code_tick_angle(gs_spec(tick_angle_y = 90)),
               "axis.text.y = element_text(angle = 90, hjust = 0.5", fixed = TRUE)

  # Both axes come out as one theme() call.
  both <- gs_code_tick_angle(gs_spec(tick_angle_x = 45, tick_angle_y = 30))
  expect_length(both, 1L)
  expect_match(both, "axis.text.x", fixed = TRUE)
  expect_match(both, "axis.text.y", fixed = TRUE)

  # An angle that is not on the list is not an angle.
  expect_length(gs_code_tick_angle(gs_spec(tick_angle_x = 17)), 0L)
  expect_length(gs_code_tick_angle(gs_spec(tick_angle_x = NA)), 0L)
})

test_that("the turned labels are still turned in the figure that is drawn", {
  d <- data.table::data.table(arm = factor(c("A", "A", "B", "B")),
                              bp = c(1, 2, 3, 4))
  spec <- gs_spec(plot_type = "Boxplot", x = "arm", y = "bp",
                  tick_angle_x = 45, tick_angle_y = 90)
  code <- gs_code_plot(spec, "d")
  # theme_bw() would put them back flat if it came second.
  expect_lt(grep("theme_bw", code), grep("axis.text.x", code))
  p <- gs_eval_plot(code, d)
  expect_equal(p$theme$axis.text.x$angle, 45)
  expect_equal(p$theme$axis.text.y$angle, 90)
  expect_s3_class(p, "ggplot")
})

test_that("a number-at-risk table still hides the axis it shares", {
  # Both settings write axis.text.x, so the blanking has to come last.
  spec <- gs_spec(plot_type = GS_KM, time = "t", event = "e", km_risk = TRUE,
                  tick_angle_x = 45)
  code <- gs_code_plot(spec, "d", shared_x = TRUE)
  expect_lt(grep("angle = 45", code), grep("axis.text.x = element_blank", code))
})
