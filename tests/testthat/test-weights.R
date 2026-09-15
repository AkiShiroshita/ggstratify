# Survey weights and the survey design: what they change, what they refuse,
# and what they report.

weighted_cohort <- function() {
  dt <- gs_prepare_data(epi_cohort)
  set.seed(3)
  dt[, w := round(stats::runif(.N, 0.5, 40), 2)]
  dt[]
}

# Draws a figure the way the app does -- the design data first, the design
# built over it, the layer rows set aside after -- and hands back the
# environment it ran in, so that the data the figure was drawn from can be read
# back as well as the figure.
run_figure <- function(spec, d, facet_strata = FALSE) {
  dd <- gs_design_data(d, spec)
  data <- dd[gs_complete_layers(dd, gs_layer_vars(spec))]
  if (facet_strata) {
    data <- gs_long_strata(data, spec$strat_vars, gs_data_cols(spec), 10L,
                           weight = spec$weight)
  }
  env <- new.env(parent = asNamespace("ggstratify"))
  assign("d", data, envir = env)
  if (gs_uses_design(spec)) assign("des", gs_build_design(dd, spec), envir = env)
  code <- gs_code_figure(spec, "d", facet_strata = facet_strata)
  assign(".p", eval(parse(text = paste(code, collapse = "\n")), envir = env),
         envir = env)
  env
}

figure_data <- function(env) ggplot2::layer_data(get(".p", envir = env))

# --- what can be a weight -----------------------------------------------------

test_that("a survey weight is a number that is never negative", {
  expect_true(gs_is_weight_col(c(0.5, 12, NA)))
  expect_true(gs_is_weight_col(c(0L, 3L)))
  expect_false(gs_is_weight_col(c(-1, 2)))
  expect_false(gs_is_weight_col(c(0, 0)))
  expect_false(gs_is_weight_col(c(1, Inf)))
  expect_false(gs_is_weight_col(c(NA_real_, NA_real_)))
  expect_false(gs_is_weight_col(factor(c("1", "2"))))
  expect_false(gs_is_weight_col(as.Date(c("2021-01-01", "2021-01-02"))))

  info <- gs_classify_vars(weighted_cohort())
  expect_true("w" %in% gs_vars_of(info, "weight"))
  expect_false("sex" %in% gs_vars_of(info, "weight"))
  choices <- gs_selector_choices(info)
  expect_equal(choices$weight, c(GS_NONE, gs_vars_of(info, "weight")))
  # A stratum or a cluster can be coded any way at all.
  expect_true(all(c("site", "id") %in% choices$design_strata))
  expect_true(all(c("site", "id") %in% choices$design_cluster))
})

test_that("the bundled cohort carries a weight to try the controls on", {
  cohort <- gs_prepare_data(epi_cohort)
  expect_true("svy_weight" %in% gs_vars_of(gs_classify_vars(cohort), "weight"))
  expect_false(anyNA(epi_cohort$svy_weight))
  # Severe presentations were over-sampled, so each stands for fewer
  # admissions, and the weighted cohort dies less often than the sample does.
  by_severity <- tapply(epi_cohort$svy_weight, epi_cohort$severity, mean)
  expect_gt(by_severity[["Mild"]], by_severity[["Severe"]])
  expect_lt(stats::weighted.mean(epi_cohort$death, epi_cohort$svy_weight),
            mean(epi_cohort$death))

  spec <- gs_spec(plot_type = GS_DOT, x = "treatment", y = "crp",
                  weight = "svy_weight", design_strata = "site",
                  err_type = "normal")
  expect_length(gs_validate_spec(spec, gs_classify_vars(cohort)), 0L)
  expect_length(gs_validate_design(cohort, spec), 0L)
  expect_no_error(ggplot2::ggplot_build(get(".p", envir = run_figure(spec, cohort))))
})

test_that("a dotplot, and a weight that is not one, are refused", {
  dt <- weighted_cohort()
  dt[, neg := w - 20]
  info <- gs_classify_vars(dt)

  expect_match(gs_validate_spec(
    gs_spec(plot_type = "Dotplot", y = "bmi", weight = "w"), info),
    "cannot carry a survey weight", all = FALSE)
  expect_match(gs_validate_spec(
    gs_spec(plot_type = "Boxplot", y = "bmi", weight = "neg"), info),
    "never negative", all = FALSE)
  # Checked before a survival curve's own checks return.
  expect_match(gs_validate_spec(
    gs_spec(plot_type = GS_KM, time = "fu_days", event = "death",
            weight = "sex"), info),
    "never negative", all = FALSE)

  expect_length(gs_validate_spec(
    gs_spec(plot_type = "Histogram", y = "age", weight = "w"), info), 0L)
  expect_length(gs_validate_spec(
    gs_spec(plot_type = GS_KM, time = "fu_days", event = "death",
            weight = "w"), info), 0L)
})

test_that("a smoother is weighted, and its band is not drawn with a weight", {
  spec <- gs_spec(plot_type = "Scatter", x = "age", y = "bmi", weight = "w",
                  smooth = TRUE, smooth_se = TRUE)
  expect_false(spec$smooth_se)
  expect_true("w" %in% gs_spec_cols(spec))
  expect_match(gs_code_smooth(spec),
               "geom_smooth(aes(weight = w), method = \"loess\"", fixed = TRUE)
  # A point is one row, so the figure itself does not map the weight.
  expect_false(grepl("weight", gs_code_plot(spec, "d")[1L], fixed = TRUE))

  # Unweighted, the smoother is what it always was.
  plain <- gs_spec(plot_type = "Scatter", x = "age", y = "bmi", smooth = TRUE,
                   smooth_se = TRUE)
  expect_true(plain$smooth_se)
  expect_match(gs_code_smooth(plain), "^geom_smooth\\(method")
})

# --- the rows, and the people they stand for ----------------------------------

test_that("a row with no weight is excluded, and counted", {
  dt <- weighted_cohort()
  dt[1:7, w := NA]
  spec <- gs_spec(plot_type = "Boxplot", y = "bmi", weight = "w",
                  strat_vars = "sex")
  expect_equal(gs_exclude_vars(spec), c("sex", "w"))
  expect_equal(sum(vapply(gs_split_strata(dt, spec), nrow, integer(1L))),
               nrow(dt) - 7L)
  miss <- gs_missing_report(dt, gs_exclude_vars(spec))
  expect_equal(miss$n_missing[miss$var == "w"], 7L)

  # The script says so as it runs, and runs without ggstratify.
  script <- gs_code_script(gs_spec(plot_type = "Histogram", y = "age",
                                   weight = "w", data_name = "dtw"))
  expect_match(script, 'layer_vars <- c("w")', fixed = TRUE)
  expect_false(grepl("ggstratify::", script, fixed = TRUE))
  env <- new.env(parent = globalenv())
  assign("dtw", as.data.frame(dt), envir = env)
  expect_message(p <- eval(parse(text = script), envir = env),
                 "Excluded 7 row")
  expect_s3_class(p, "ggplot")
})

test_that("a weighted stratum reports its rows and the people they stand for", {
  expect_equal(gs_label_n("sex: M", 30), "sex: M (N = 30)")
  expect_equal(gs_label_n("sex: M", 30, 12345.6),
               "sex: M (N = 30; weighted N = 12,346)")
  expect_equal(gs_format_weighted_n(c(0.4, 1234567)), c("0", "1,234,567"))

  dt <- weighted_cohort()
  st <- gs_strata_table(dt, c("sex", "site"), 10L, weight = "w")
  expect_equal(names(st), c("var", "level", "label", "file", "n", "n_w",
                            "keep", "status"))
  expect_equal(st$n_w[st$label == "sex: Male"], sum(dt$w[dt$sex == "Male"]))
  # A level nobody is in stands for nobody.
  expect_equal(st$n_w[st$level == "Site D"], 0)
  # The minimum is still a number of rows.
  expect_equal(st$keep, gs_strata_table(dt, c("sex", "site"), 10L)$keep)

  crossed <- gs_strata_table(dt, c("sex", "site"), 0L, "crossed", "w")
  at <- dt$sex == "Female" & dt$site == "Site B"
  expect_equal(crossed$n_w[crossed$label == "sex: Female | site: Site B"],
               sum(dt$w[at]))
  expect_equal(sum(crossed$n_w), sum(dt$w))

  # Unweighted, the table is what it always was.
  expect_false("n_w" %in% names(gs_strata_table(dt, "sex", 0L)))

  expect_equal(gs_title_literal(gs_spec(), "sex_Male", 291L, 5987.2),
               "\"sex_Male (N = 291; weighted N = 5,987)\"")
})

test_that("a weight called w is not confused with the helpers' own w", {
  # The sums are taken outside data.table's `[` for exactly this reason: inside
  # it, a column named `w` would be read in place of the vector of weights.
  dt <- data.table::data.table(g = c("a", "a", "b"), w = c(1, 2, 10))
  expect_equal(gs_strata_table(dt, "g", 0L, weight = "w")$n_w, c(3, 10))
  expect_equal(gs_strata_table(dt, "g", 0L, "crossed", weight = "w")$n_w,
               c(3, 10))
  long <- gs_long_strata(dt, "g", "w", 0L, weight = "w")
  expect_equal(levels(long$.strat_label),
               c("g: a (N = 2; weighted N = 3)", "g: b (N = 1; weighted N = 10)"))
})

test_that("the panels of a weighted figure carry both counts, in app and script", {
  dt <- weighted_cohort()
  long <- gs_long_strata(dt, c("sex", "severity"), c("bmi", "w"), 0L,
                         weight = "w")
  severe <- dt$severity == "Severe"
  expect_true(gs_label_n("severity: Severe", sum(severe), sum(dt$w[severe])) %in%
                levels(long$.strat_label))

  # The strip the generated add_facet_n() writes reads exactly as the app's.
  spec <- gs_spec(plot_type = "Boxplot", x = "treatment", y = "bmi",
                  weight = "w", facet = "site")
  env <- run_figure(spec, dt)
  strips <- levels(get("d", envir = env)[[GS_FACET_COL]])
  at_a <- dt$site == "Site A"
  expect_equal(strips[1L], gs_label_n("site: Site A", sum(at_a), sum(dt$w[at_a])))
  expect_equal(strips[4L], "site: Site D (N = 0; weighted N = 0)")
})

# --- figures drawn through the weight aesthetic -------------------------------

test_that("a weighted histogram counts weights rather than rows", {
  dt <- weighted_cohort()
  spec <- gs_spec(plot_type = "Histogram", y = "age", weight = "w",
                  binwidth = 5)
  code <- gs_code_plot(spec, "d")
  expect_match(code[1L], "aes(x = age, weight = w)", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), 'y = "Weighted count"',
               fixed = TRUE)
  bins <- ggplot2::layer_data(gs_eval_plot(gs_code_figure(spec, "d"), dt))
  expect_equal(sum(bins$count), sum(dt$w))
})

test_that("a box, a violin and a density are drawn from the weighted rows", {
  dt <- weighted_cohort()
  for (type in c("Boxplot", "Violin", "Density")) {
    spec <- gs_spec(plot_type = type, x = "treatment", y = "bmi", weight = "w")
    expect_match(gs_code_plot(spec, "d")[1L], "weight = w", fixed = TRUE,
                 info = type)
    # ggplot2 warns that a violin's quantiles are not weighted; none are drawn.
    weighted <- suppressWarnings(ggplot2::layer_data(
      gs_eval_plot(gs_code_figure(spec, "d"), dt)))
    plain <- ggplot2::layer_data(gs_eval_plot(gs_code_figure(
      gs_spec(plot_type = type, x = "treatment", y = "bmi"), "d"), dt))
    expect_false(isTRUE(all.equal(weighted, plain)), info = type)
  }
})

# --- Dot + Error, estimated by survey -----------------------------------------

err_data_w <- function() {
  set.seed(7)
  data.table::data.table(
    arm = factor(rep(c("Control", "Treated"), each = 60)),
    bp = c(stats::rnorm(60, 140, 12), stats::rnorm(60, 132, 12)),
    died = c(stats::rbinom(60, 1, 0.30), stats::rbinom(60, 1, 0.12)),
    w = stats::runif(120, 0.2, 5)
  )
}

dot_bars <- function(d, ...) {
  figure_data(run_figure(gs_spec(plot_type = GS_DOT, x = "arm", weight = "w",
                                 ...), d))
}

weights_design <- function(d) {
  survey::svydesign(ids = ~1, weights = ~w, data = as.data.frame(d))
}

test_that("a weighted mean's bar is survey's standard error or t interval", {
  d <- err_data_w()
  des <- weights_design(d)
  se <- dot_bars(d, y = "bp", err_type = "se")
  ci <- dot_bars(d, y = "bp", err_type = "normal", err_level = 0.9)
  for (i in 1:2) {
    m <- survey::svymean(~bp, des[d$arm == levels(d$arm)[i], ])
    expect_equal(se$y[i], unname(coef(m)))
    expect_equal(se$ymax[i] - se$y[i], as.numeric(survey::SE(m)))
    # On the whole design's degrees of freedom, as for any subpopulation.
    ref <- confint(m, level = 0.9, df = survey::degf(des))
    expect_equal(c(ci$ymin[i], ci$ymax[i]), as.numeric(ref))
  }
})

test_that("a weighted proportion's bar is svyciprop's interval", {
  d <- err_data_w()
  des <- weights_design(d)
  exact <- dot_bars(d, y = "died", err_type = "exact", err_event = "1")
  wilson <- dot_bars(d, y = "died", err_type = "wilson", err_event = "1")
  for (i in 1:2) {
    part <- des[d$arm == levels(d$arm)[i], ]
    beta <- confint(survey::svyciprop(~died, part, method = "beta",
                                      df = survey::degf(des)))
    score <- confint(survey::svyciprop(~died, part, method = "wilson",
                                       df = survey::degf(des)))
    expect_equal(c(exact$ymin[i], exact$ymax[i]), as.numeric(beta))
    expect_equal(c(wilson$ymin[i], wilson$ymax[i]), as.numeric(score))
  }
})

test_that("a weighted proportion of 0 or 1 still has an interval", {
  # svyciprop() returns NaN there, because the design variance is zero.
  for (value in c(0L, 1L)) {
    d <- data.table::data.table(arm = factor(rep("A", 25L)),
                                died = rep(value, 25L),
                                w = stats::runif(25L, 0.5, 3))
    for (type in GS_ERR_PROP_TYPES) {
      bars <- dot_bars(d, y = "died", err_type = type, err_event = "1")
      expect_equal(bars$y, value, info = type)
      expect_false(anyNA(c(bars$ymin, bars$ymax)), info = type)
      expect_gte(bars$ymin, 0)
      expect_lte(bars$ymax, 1)
      expect_gt(bars$ymax - bars$ymin, 0)
    }
  }
})

test_that("a single weighted observation gets a point and no interval", {
  one <- data.table::data.table(arm = factor(c("A", "B", "B")),
                                bp = c(140, 120, 130), w = c(2, 1, 1))
  bars <- dot_bars(one, y = "bp", err_type = "normal")
  expect_equal(bars$y[1L], 140)
  expect_true(is.na(bars$ymin[1L]))
})

test_that("a weighted Dot + Error figure draws however it is split", {
  dt <- weighted_cohort()
  for (spec in list(
    gs_spec(plot_type = GS_DOT, y = "bmi", weight = "w", err_type = "normal"),
    gs_spec(plot_type = GS_DOT, x = "treatment", y = "bmi", weight = "w",
            group = "sex", facet = "site", jitter = TRUE, palette = "Set1"),
    gs_spec(plot_type = GS_DOT, x = "treatment", y = "death", weight = "w",
            err_type = "wilson", err_event = "1", jitter = TRUE)
  )) {
    code <- gs_code_figure(spec, "d")
    expect_silent(parse(text = paste(code, collapse = "\n")))
    expect_no_warning(ggplot2::ggplot_build(get(".p", envir = run_figure(spec, dt))))
  }
  # The axis says what the point is, not `.y`.
  expect_match(gs_code_labs(gs_spec(plot_type = GS_DOT, x = "arm", y = "bp",
                                    weight = "w"), "colour", FALSE),
               'y = "Weighted mean of bp"', fixed = TRUE)

  # The all-figures preview estimates each panel within its own panel.
  spec <- gs_spec(plot_type = GS_DOT, x = "treatment", y = "bmi", weight = "w",
                  strat_vars = c("sex", "severity"))
  code <- gs_code_figure(spec, "d", facet_strata = TRUE)
  expect_match(paste(code, collapse = "\n"), '"treatment", ".strat_label"',
               fixed = TRUE)
  expect_no_error(ggplot2::ggplot_build(
    get(".p", envir = run_figure(spec, dt, facet_strata = TRUE))))
})

# --- strata and clusters ------------------------------------------------------

cluster_data <- function() {
  set.seed(11)
  d <- data.table::data.table(
    stratum = rep(c("N", "S", "E"), each = 96L),
    psu = rep(rep(1:8, each = 12L), 3L)
  )
  # A strong cluster effect, so that clustering visibly widens an interval.
  effect <- stats::rnorm(24L, sd = 5)
  d[, bp := 130 + effect[(match(stratum, c("N", "S", "E")) - 1L) * 8L + psu] +
      stats::rnorm(.N, sd = 3)]
  d[, arm := factor(sample(c("A", "B"), .N, TRUE))]
  d[, died := stats::rbinom(.N, 1L, 0.3)]
  d[, w := stats::runif(.N, 1, 10)]
  # A layer that holds only some of the clusters: half of one stratum's.
  d[, region := factor(ifelse(stratum == "N" & psu <= 4L, "Upper", "Lower"))]
  d[]
}

cluster_design <- function(d) {
  survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w, nest = TRUE,
                    data = as.data.frame(d))
}

test_that("the design names its strata and clusters, and nests one in another", {
  both <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
                  design_strata = "stratum", design_cluster = "psu")
  expect_equal(gs_code_design_call(both)[2L], paste0(
    "des <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w, ",
    "nest = TRUE,"))
  expect_equal(gs_code_design_call(gs_spec(plot_type = GS_DOT, y = "bp",
                                           weight = "w"))[2L],
               "des <- survey::svydesign(ids = ~1, weights = ~w,")
  expect_equal(gs_code_design_call(gs_spec(plot_type = GS_DOT, y = "bp",
                                           weight = "w",
                                           design_cluster = "psu"))[2L],
               "des <- survey::svydesign(ids = ~psu, weights = ~w,")

  # Without a weight there is no design to stratify.
  plain <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp",
                   design_strata = "stratum", design_cluster = "psu")
  expect_equal(c(plain$design_strata, plain$design_cluster), c("", ""))

  # A histogram reads the weight through ggplot2 and builds no design, but a
  # row with no stratum is still left out, so that N does not change with the
  # plot type.
  hist <- gs_spec(plot_type = "Histogram", y = "bp", weight = "w",
                  design_strata = "stratum")
  expect_false(gs_uses_design(hist))
  expect_false(grepl("svydesign", gs_code_script(hist), fixed = TRUE))
  expect_equal(gs_exclude_vars(hist), c("w", "stratum"))
})

test_that("a clustered bar is the design's estimate for that subpopulation", {
  d <- cluster_data()
  des <- cluster_design(d)
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
                  design_strata = "stratum", design_cluster = "psu",
                  err_type = "normal")
  bars <- figure_data(run_figure(spec, d))
  for (i in 1:2) {
    m <- survey::svymean(~bp, des[d$arm == levels(d$arm)[i], ])
    ref <- confint(m, df = survey::degf(des))
    expect_equal(c(bars$ymin[i], bars$ymax[i]), as.numeric(ref))
  }
  # Clusters that resemble themselves carry less information than their rows
  # suggest, which is what the wider interval says.
  weights_only <- figure_data(run_figure(
    gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
            err_type = "normal"), d))
  expect_true(all(bars$ymax - bars$ymin > weights_only$ymax - weights_only$ymin))

  # A proportion through the same design.
  exact <- figure_data(run_figure(
    gs_spec(plot_type = GS_DOT, x = "arm", y = "died", weight = "w",
            design_strata = "stratum", design_cluster = "psu",
            err_type = "exact", err_event = "1"), d))
  ref <- confint(survey::svyciprop(~died, des[d$arm == "A", ], method = "beta",
                                   df = survey::degf(des)))
  expect_equal(c(exact$ymin[1L], exact$ymax[1L]), as.numeric(ref))
})

test_that("a figure's standard errors come from the whole sample's design", {
  d <- cluster_data()
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
                  design_strata = "stratum", design_cluster = "psu",
                  strat_vars = "region", min_n = 0L, data_name = "svy")
  st <- gs_strata_table(d, "region", 0L, weight = "w")
  script <- gs_code_script(spec, st[st$level == "Upper", ])
  # The design is built before the data is cut down to the figure.
  expect_lt(regexpr("survey::svydesign", script, fixed = TRUE),
            regexpr("d <- dt[", script, fixed = TRUE))

  env <- new.env(parent = globalenv())
  assign("svy", as.data.frame(d), envir = env)
  eval(parse(text = script), envir = env)
  est <- get("est", envir = env)

  des <- cluster_design(d)
  in_figure <- d$region == "Upper" & d$arm == "A"
  ref <- survey::svymean(~bp, des[in_figure, ])
  expect_equal(est$.ymax[est$arm == "A"] - est$.y[est$arm == "A"],
               as.numeric(survey::SE(ref)))

  # A design rebuilt on the figure's rows alone has lost the clusters the
  # figure does not hold, and gives a different standard error.
  sub <- as.data.frame(d[d$region == "Upper"])
  rebuilt <- cluster_design(sub)
  wrong <- survey::svymean(~bp, rebuilt[sub$arm == "A", ])
  expect_false(isTRUE(all.equal(as.numeric(survey::SE(ref)),
                                as.numeric(survey::SE(wrong)))))
})

test_that("a row with no stratum or cluster is excluded before the design", {
  d <- cluster_data()
  d[c(3L, 50L), psu := NA]
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
                  design_strata = "stratum", design_cluster = "psu",
                  data_name = "svy")
  dd <- gs_design_data(d, spec)
  expect_equal(nrow(dd), nrow(d) - 2L)
  expect_equal(dd$.svy_row, seq_len(nrow(d) - 2L))
  des <- gs_build_design(dd, spec)
  expect_s3_class(des, "survey.design2")
  expect_equal(nrow(des), nrow(d) - 2L)

  script <- gs_code_script(spec)
  expect_match(script, 'design_vars <- c("w", "stratum", "psu")', fixed = TRUE)
  env <- new.env(parent = globalenv())
  assign("svy", as.data.frame(d), envir = env)
  expect_message(eval(parse(text = script), envir = env),
                 "Excluded 2 row")
})

test_that("a stratum with a single cluster is refused, by name", {
  spec <- gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
                  design_strata = "stratum", design_cluster = "psu")
  expect_length(gs_validate_design(cluster_data(), spec), 0L)

  d <- cluster_data()
  d[stratum == "E", psu := 1L]
  expect_match(gs_validate_design(d, spec),
               "Sampling stratum 'E' of 'stratum' has only one cluster",
               fixed = TRUE)
  # A survival curve without its band estimates no variance, and is drawn.
  km <- gs_spec(plot_type = GS_KM, time = "t", event = "e", weight = "w",
                design_strata = "stratum", design_cluster = "psu")
  expect_length(gs_validate_design(d, km), 0L)
  km$km_ci <- TRUE
  expect_match(gs_validate_design(d, km), "only one cluster", fixed = TRUE)

  # And one variable cannot be both.
  expect_match(gs_validate_spec(
    gs_spec(plot_type = GS_DOT, x = "arm", y = "bp", weight = "w",
            design_strata = "psu", design_cluster = "psu")),
    "cannot be both", all = FALSE)
})

# --- Kaplan-Meier, estimated by survey ----------------------------------------

km_w_spec <- function(...) {
  gs_spec(plot_type = GS_KM, time = "fu_days", event = "death", weight = "w",
          ...)
}

test_that("a weighted curve is svykm()'s, with its band and censoring marks", {
  dt <- weighted_cohort()
  env <- run_figure(km_w_spec(km_ci = TRUE), dt)
  km <- get("km", envir = env)

  des <- survey::svydesign(ids = ~1, weights = ~w, data = as.data.frame(dt))
  fit <- survey::svykm(survival::Surv(fu_days, death) ~ 1, des, se = TRUE)
  # Tied events are listed once each, a step apiece; the curve at that time is
  # the last of them, which is also the one confint() on the fit reads.
  last <- !duplicated(fit$time, fromLast = TRUE)
  at <- match(fit$time[last], km$.time)
  expect_false(anyNA(at))
  expect_equal(km$.surv[at], unname(fit$surv[last]))
  band <- confint(fit, parm = fit$time[last], level = 0.95)
  expect_equal(km$.lower[at], unname(band[, 1L]))
  expect_equal(km$.upper[at], unname(band[, 2L]))

  # Every observed time is a step, and every censored row is marked once.
  expect_equal(km$.time, c(0, sort(unique(dt$fu_days))))
  expect_equal(sum(km$.ncens), sum(dt$death == 0L))
  expect_true(all(diff(km$.surv) <= 0))
})

test_that("a weighted curve draws grouped, panelled, banded and with no events", {
  dt <- weighted_cohort()
  for (spec in list(km_w_spec(),
                    km_w_spec(group = "treatment", km_ci = TRUE),
                    km_w_spec(group = "treatment", facet = "sex",
                              km_ci = TRUE, palette = "Set1"))) {
    code <- gs_code_figure(spec, "d")
    expect_silent(parse(text = paste(code, collapse = "\n")))
    expect_no_error(ggplot2::ggplot_build(get(".p", envir = run_figure(spec, dt))))
  }
  # The band is asked for only when it is drawn: it is the slow part.
  expect_false(grepl("se = TRUE", paste(gs_code_prep(km_w_spec(), "d"),
                                        collapse = "\n"), fixed = TRUE))

  dt[sex == "Male", death := 0L]
  env <- run_figure(km_w_spec(group = "sex", km_ci = TRUE), dt)
  km <- get("km", envir = env)
  expect_true(all(km$.surv[km$sex == "Male"] == 1))
})

test_that("a clustered survival curve and its band draw, in app and script", {
  d <- cluster_data()
  d[, t := stats::rexp(.N, 0.1)]
  d[, e := stats::rbinom(.N, 1L, 0.7)]
  spec <- gs_spec(plot_type = GS_KM, time = "t", event = "e", weight = "w",
                  design_strata = "stratum", design_cluster = "psu",
                  km_ci = TRUE, km_risk = TRUE, group = "arm",
                  facet = "region", data_name = "svy")
  env <- run_figure(spec, d)
  expect_no_error(patchwork::patchworkGrob(get(".p", envir = env)))

  script <- gs_code_script(spec)
  e2 <- new.env(parent = globalenv())
  assign("svy", as.data.frame(d), envir = e2)
  expect_s3_class(eval(parse(text = script), envir = e2), "patchwork")
})

test_that("the weighted number-at-risk table counts rows and weights", {
  dt <- weighted_cohort()
  spec <- km_w_spec(km_risk = TRUE, group = "treatment")
  env <- run_figure(spec, dt)
  expect_s3_class(get(".p", envir = env), "patchwork")
  expect_no_error(patchwork::patchworkGrob(get(".p", envir = env)))

  risk <- get("risk", envir = env)
  first <- risk[risk$.time == 0 & risk$treatment == "Control", ]
  control <- dt$treatment == "Control"
  expect_equal(first$.nrisk, sum(control))
  expect_equal(first$.nrisk_label,
               sprintf("%d\n(%s)", sum(control),
                       gs_format_weighted_n(sum(dt$w[control]))))
})

test_that("a weighted script stands on its own", {
  dt <- weighted_cohort()
  for (spec in list(
    gs_spec(plot_type = GS_DOT, x = "treatment", y = "bmi", weight = "w",
            facet = "severity", strat_vars = "sex", err_type = "normal",
            jitter = TRUE, data_name = "dtw"),
    km_w_spec(km_ci = TRUE, km_risk = TRUE, group = "sex", data_name = "dtw")
  )) {
    st <- gs_strata_table(dt, spec$strat_vars, 10L, weight = "w")
    script <- gs_code_script(spec, if (nrow(st)) st[1L] else NULL)
    expect_silent(parse(text = script))
    expect_false(grepl("ggstratify::", script, fixed = TRUE))
    env <- new.env(parent = globalenv())
    assign("dtw", as.data.frame(dt), envir = env)
    p <- eval(parse(text = script), envir = env)
    expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  }
})

test_that("the columns the weighted helpers work in cannot be shadowed", {
  dt <- data.table::data.table(.w = 1:2, .y = 1:2, .svy_row = 1:2, x = 1:2)
  expect_equal(names(gs_clean_names(dt)), c(".w_", ".y_", ".svy_row_", "x"))
})
