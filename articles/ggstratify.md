# Getting started with ggstratify

``` r

library(ggstratify)
```

## What is ggstratify?

Descriptive analysis often means making the same figure repeatedly for
different subgroups. `ggstratify` makes this easier: choose the
variables you want to describe and stratify by, and the app creates the
figures for you. It also gives you the R code used to create each
figure.

Everything runs locally. Your data are not uploaded anywhere.

## Before you start

`ggstratify` uses your variables as they are typed. Make sure
categorical variables are factors and that their levels are in the order
you want.

``` r

dat <- transform(
  dat,
  sex = factor(sex, levels = c("Male", "Female")),
  severity = factor(severity, levels = c("Mild", "Moderate", "Severe")),
  age = as.numeric(age)
)
```

The app takes a data object, not a file. Read your data first, check the
variables, and then pass the object to
[`ggstratify()`](https://akishiroshita.github.io/ggstratify/reference/ggstratify.md):

``` r

cohort <- read.csv("cohort.csv")
str(cohort)
ggstratify(cohort)
```

Your original data are never modified.

## Launch the app

``` r

ggstratify(epi_cohort)
```

`epi_cohort` is a simulated dataset included with the package.

``` r

str(epi_cohort)
#> 'data.frame':    600 obs. of  13 variables:
#>  $ id        : chr  "P0001" "P0002" "P0003" "P0004" ...
#>  $ age       : num  66 78 57 42 57 64 64 67 72 69 ...
#>  $ sex       : Factor w/ 2 levels "Male","Female": 1 1 2 2 2 1 2 2 2 1 ...
#>  $ site      : Factor w/ 4 levels "Site A","Site B",..: 1 2 1 1 1 1 1 1 2 1 ...
#>  $ treatment : Factor w/ 3 levels "Control","Low dose",..: 1 3 3 2 1 2 2 1 3 1 ...
#>  $ severity  : Factor w/ 3 levels "Mild","Moderate",..: 1 2 1 2 2 1 1 1 1 1 ...
#>  $ bmi       : num  23.6 23.1 24.9 20.1 25.3 21.2 23 25.6 27.6 26 ...
#>  $ crp       : num  4.4 4.6 NA 21.8 18.1 17.6 4.4 NA NA NA ...
#>  $ los_days  : num  14 10 5 15 16 12 9 10 8 10 ...
#>  $ fu_days   : num  229 65 258 289 141 4 235 365 40 365 ...
#>  $ death     : int  1 1 0 0 1 0 0 0 0 0 ...
#>  $ admit_date: Date, format: "2021-11-05" "2021-03-24" ...
#>  $ svy_weight: num  110 32.2 102.1 34.5 32 ...
```

The same function works with any data frame, tibble, data.table, or
matrix.

## Stratifying your figures

The **Layers** panel controls how figures are split.

- **No layer:** one figure.
- **One panel variable:** several panels in one figure.
- **Additional stratification variables:** separate figures.

For example, you could describe `D`, show panels by `C`, and create
separate figures by `A` and `B`.

When several stratification variables are selected, you can either:

- **Separate:** make figures for each variable independently.
- **Cross:** make a figure for every combination of their levels.

The **Strata** tab shows the figures that will be produced and their
sample sizes before you export them.

## Missing values and small strata

Rows missing a value for a stratification variable are excluded because
they cannot be assigned to a figure or panel. The number of excluded
rows is shown in the app and in the generated code.

Strata with fewer observations than the selected **Minimum N per
figure** are listed but not plotted. Set the minimum to `0` if you want
to include small strata. Empty factor levels are never plotted.

## What the error bar stands for

A **Dot + Error** figure draws one standard error either side of the
mean unless told otherwise:

``` r

stat_summary(fun.data = mean_se, geom = "pointrange")
```

That is a statement about how precisely the mean is known, and it is
about half as wide as the confidence interval most readers take an error
bar to be. Under **Plot options** you can ask for the interval instead,
at 90, 95 or 99 percent. For a measurement it is the t interval – the
one [`t.test()`](https://rdrr.io/r/stats/t.test.html) reports:

``` r

mean_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- if (n) mean(x) else NA_real_
  if (n < 2L) return(data.frame(y = m, ymin = NA_real_, ymax = NA_real_))
  half <- qt(1 - (1 - conf) / 2, n - 1L) * sd(x) / sqrt(n)
  data.frame(y = m, ymin = m - half, ymax = m + half)
}
```

### A proportion is not a measurement

A Y variable coded 0 and 1 – died or did not, readmitted or not – is a
proportion, and a mean plus or minus a standard error describes it
badly. Near 0 or 1 the bar runs outside the range a proportion can take,
and in a stratum where nobody had the outcome it has no width at all and
claims certainty.

Such a column is not continuous, so it appears in the Y list only for
this figure, and the bars offered narrow to the two that apply to it.

It can be written any of the ways R keeps an outcome – `0`/`1`,
`TRUE`/`FALSE`, a two-level factor, two character values – and **Count
as the outcome** says which of the two values is being counted. It
defaults to the second, which is R’s own convention, and you will want
to change it for a factor declared `c("Yes", "No")`.

The counting happens in the figure rather than to the data:

``` r

ggplot(d, aes(x = severity, y = as.integer(outcome == "Died"))) +
  stat_summary(fun.data = prop_ci_wilson, fun.args = list(conf = 0.95),
               geom = "pointrange") +
  labs(y = "Proportion outcome = Died")
```

That is not a stylistic choice. `ggplot2` puts a non-numeric Y on a
discrete scale, so an outcome left as a factor would reach the interval
as the level number – 1 and 2 – and come back as a proportion above one,
with a `NaN` from the square root of a negative variance as the only
sign that anything went wrong. Comparing inside the
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) avoids that,
keeps the column as you typed it, and puts what was counted on the
screen.

Two intervals are offered for that case. **Exact (Clopper-Pearson)**
inverts the binomial test, so it never covers less often than it claims:

``` r

prop_ci_exact <- function(x, conf = 0.95) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  ci <- binom.test(sum(x), length(x), conf.level = conf)$conf.int
  data.frame(y = mean(x), ymin = ci[1], ymax = ci[2])
}
```

**Wilson** inverts the score test, and is the better-centred of the two
at small n. It is the interval `prop.test(correct = FALSE)` gives:

``` r

prop_ci_wilson <- function(x, conf = 0.95) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  n <- length(x); p <- mean(x)
  z <- qnorm(1 - (1 - conf) / 2)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  data.frame(y = p, ymin = max(0, centre - half), ymax = min(1, centre + half))
}
```

Which to prefer is a judgement – exact is conservative, Wilson is better
centred – so both are offered and neither is chosen for you. Whichever
you pick, the function that computes it is printed above the figure in
the **R-code** tab, so the interval in the figure is one a reader can
check.

`death` in the bundled `epi_cohort` is coded 0 and 1, so
`ggstratify(epi_cohort)` is enough to try this.

## Survey weights

Data from a survey rarely describe the people in them one to one: each
row stands for as many people as its weight. Choose the weight column
under **Survey weight** in **Describe**, and a histogram is drawn the
way you would draw it by hand:

``` r

ggplot(d, aes(x = age, weight = survey_weight)) +
  geom_histogram()
```

Densities, boxplots and violins take the weight the same way, and a
smoother is fitted with it. A dotplot draws one dot per row, so it
cannot be weighted and is refused rather than drawn as though the weight
were not there.

Every count is then reported twice: `N`, the rows a figure or panel is
drawn from, and the weighted N, the sum of their weights. The first says
how far to trust the figure’s shape, the second how many people it
describes.

A bar or a band is a different matter, because a weighted standard error
is not an unweighted one computed on weighted data. The bar on a **Dot +
Error** figure and the band on a Kaplan-Meier curve are estimated with
the ‘survey’ package, and the functions that do it are printed on the
**R-code** tab:

``` r

dt[, .svy_row := .I]
des <- survey::svydesign(ids = ~ward, strata = ~site, weights = ~svy_weight,
                         nest = TRUE, data = as.data.frame(dt))

survey::svymean(~crp, des[rows, ])                      # a mean and its SE
survey::svyciprop(~died, des[rows, ], method = "beta")  # a proportion
survey::svykm(survival::Surv(fu_days, death) ~ 1, des[rows, ], se = TRUE)
```

**Sampling strata** and **Clusters (primary sampling units)**, under the
weight, complete the design; either can be left empty. A cluster ID is
read within its stratum, so ward 1 of one site is not ward 1 of the
next.

The design is built once, over the whole sample, before any row is set
aside for a panel or a figure. Each point and each curve is then a
subpopulation of it – `rows` above – so its standard error counts every
stratum and cluster in the sample, the ones it holds no rows of
included, and its interval is read on the whole design’s degrees of
freedom. A design rebuilt on one figure’s rows would have forgotten the
clusters that figure happens not to hold, and would give a different
answer. The exact proportion interval is Korn and Graubard’s, the survey
counterpart of Clopper-Pearson; Wilson is `svyciprop()`’s own.

A stratum with a single cluster has nothing to estimate a variance from.
The app names it and asks you to merge it with a neighbouring stratum
first.

`svy_weight` in `epi_cohort` reads the cohort as a sample in which
severe presentations were over-sampled, so the weighted figures describe
a mostly mild population; `site` can stand in for the strata.

## Turning the tick labels

Twelve month names along one axis do not fit side by side, and `ggplot2`
draws them overlapping rather than dropping any. **Appearance** turns
the labels on either axis by 30, 45, 60 or 90 degrees:

``` r

theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```

It is emitted after the theme, because a theme replaces the element
rather than merging into it –
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
afterwards would put the labels back flat.

## Deriving a variable

The **Derive a variable** panel makes a new categorical variable out of
one you already have, using:

- **Quantiles**
- **Equal-width bins**
- **Custom cut points**
- **Missing vs observed**
- **Time resolution**

For example, you can create an age group at 65 years:

``` r

dt[, age_cat := cut(age, breaks = c(-Inf, 65, Inf))]
```

The new variable can then be used like any other categorical variable.

### Missing vs observed

The first three methods read a continuous variable’s values. The fourth
reads only whether there is a value, so it applies to a variable of any
type – a factor, a character column or a date as much as a number:

``` r

dt[, crp_missing := factor(is.na(crp), levels = c(FALSE, TRUE),
                           labels = c("Observed", "Missing"))]
```

Stratify by it to describe another variable within each group: how the
people whose CRP was never measured differ from the people whose CRP
was. Every row lands in one of the two groups, so none is excluded – the
rows that a layer variable’s own missingness would have dropped are the
ones this puts on the screen.

Describing the variable itself within its own missingness is the one
arrangement to avoid: the Missing figure would be drawn from rows that
have no value for it. The app says so under **Layers** when you ask for
that.

A variable with no missing values, or with nothing but missing values,
gives one group rather than two and is refused with a note saying which.

`crp` in the bundled `epi_cohort` is left incomplete on purpose so that
this can be tried straight away: 122 of the 600 patients were never
tested, and the milder the presentation the more often the test was
skipped.

### Time resolution

A date is rarely a layer as it stands. `admit_date` in `epi_cohort` runs
over three calendar years and has several hundred distinct values, so
stratifying by it would ask for several hundred figures. The fifth
method reads it at whatever resolution the question is being asked at.

Two families, answering different questions. A **calendar period** keeps
the year, so every row in March 2021 lands together:

``` r

dt[, admit_month := as.IDate(cut(as.IDate(admit_date), breaks = "month"))]
```

The result is still a date. That is the point of it: a trend drawn
against this column is spaced by elapsed time rather than by level
number, and its axis can still be given date ticks.

A **position in the cycle** drops the year and pools every March since
the data began, which is what a seasonal pattern is:

``` r

dt[, admit_month := factor(month.abb[month(admit_date)], levels = month.abb)]
```

The levels are written out rather than sorted, because sorting the
months of the year puts April first. Days of the week run Monday to
Sunday for the same reason.

A season is the same idea over three months at a time, and you choose
the month it starts in:

``` r

dt[, admit_season := factor(
  c("Spring", "Summer", "Fall", "Winter")[
    ((month(admit_date) - 3L) %% 12L) %/% 3L + 1L],
  levels = c("Spring", "Summer", "Fall", "Winter"))]
```

March gives the northern hemisphere and September the southern. The
order spring, summer, fall, winter does not change with it.

Describe a measurement within the result and the seasonal pattern is a
figure: `admit_date` read by season, `los_days` as a boxplot, and the
four panels come out in season order. In `epi_cohort` the winter cases
are the more severe ones, so the difference is a real one rather than
noise.

Two things this method will not do. A resolution the column cannot
answer is refused with the reason rather than silently producing one
group: an hour of the day taken from a plain date would be midnight for
every row, and a month taken from a time of day has no calendar to fall
in. And the application does not aggregate before it plots – a boxplot
by month shows the distribution of the observations in each month, not
the distribution of monthly means. If you want the latter, summarise the
data yourself first and pass the result to
[`ggstratify()`](https://akishiroshita.github.io/ggstratify/reference/ggstratify.md).

## A date on the X axis

A `Line` plot drawn against a date has its own controls under
**Appearance**: how far apart the ticks are, and what each one reads.

``` r

ggplot(d, aes(x = admit_date, y = los_days)) +
  geom_line(alpha = 0.6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year") +
  theme_bw()
```

The values plotted do not change; only the ticks over them. A column
that really holds dates is therefore drawn day by day and read year by
year, and with no axis label typed the axis is titled by the unit rather
than by the column’s name. The tick text follows your R session’s
language, so `%b` reads `Mar` under an English locale and differently
under another.

The numeric **X axis from** and **X axis to** boxes cannot hold a date,
so they are ignored while the X variable is one.

## Figure types

`ggstratify` supports several common descriptive plots, including:

- Boxplots
- Histograms
- Bar plots
- Scatter plots
- Line plots
- Kaplan-Meier curves

For line plots, choose a time variable and measurement, and optionally
specify an ID to draw one line per subject. A LOWESS smoother can also
be added.

For Kaplan-Meier curves, select the follow-up time and event variables.
Optional confidence intervals, censoring marks, and risk tables are
available.

All plots are calculated within the selected panels and strata.

## Changing the axis range

**Appearance** lets you set the X- and Y-axis ranges.

The range only zooms the figure; it does not remove observations from
the analysis. This means that summary statistics such as boxplot medians
remain unchanged.

## Generated R code

The **R-code** tab shows the code used to create the figure currently
shown.

The code includes the necessary data preparation, stratification, and
`ggplot2` commands, so you can copy it into your own analysis.

For example:

``` r

library(data.table)
library(ggplot2)

dt <- as.data.table(epi_cohort)

d <- dt[sex == "Male"]

p <- ggplot(d, aes(x = treatment, y = los_days)) +
  geom_boxplot() +
  theme_bw()

p
```

The preview, exported figures, and R-code tab all use the same code
generator, so the generated code reproduces the figure shown in the app.

## Exporting figures

Click **Export all figures** to save the figures listed in the
**Strata** tab.

You can export:

- **PNG** for raster images
- **SVG** for vector graphics

The exported figures use the full dataset, even when a large dataset is
sampled for the on-screen preview.

## In short

The basic workflow is:

1.  Prepare and check your data.
2.  Run `ggstratify(your_data)`.
3.  Choose the variable to describe.
4.  Add panels or stratification variables if needed.
5.  Adjust the appearance.
6.  Check the **Strata** tab.
7.  Copy the R code or export the figures if needed.
