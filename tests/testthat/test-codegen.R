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
