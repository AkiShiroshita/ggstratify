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
#> 'data.frame':    600 obs. of  11 variables:
#>  $ id       : chr  "P0001" "P0002" "P0003" "P0004" ...
#>  $ age      : num  66 78 57 42 57 64 64 67 72 69 ...
#>  $ sex      : Factor w/ 2 levels "Male","Female": 1 1 2 2 2 1 2 2 2 1 ...
#>  $ site     : Factor w/ 4 levels "Site A","Site B",..: 1 2 1 1 1 1 1 1 2 1 ...
#>  $ treatment: Factor w/ 3 levels "Control","Low dose",..: 1 3 3 2 1 2 2 1 3 1 ...
#>  $ severity : Factor w/ 3 levels "Mild","Moderate",..: 1 2 1 2 2 1 1 1 1 1 ...
#>  $ bmi      : num  23.6 23.1 24.9 20.1 25.3 21.2 23 25.6 27.6 26 ...
#>  $ crp      : num  4.4 4.6 8.8 21.8 18.1 17.6 4.4 6.3 1.7 3.4 ...
#>  $ los_days : num  14 10 5 15 16 12 9 10 8 10 ...
#>  $ fu_days  : num  229 65 258 289 141 4 235 365 40 365 ...
#>  $ death    : int  1 1 0 0 1 0 0 0 0 0 ...
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

## Categorizing a continuous variable

The **Categorize** panel lets you turn a continuous variable into groups
using:

- **Quantiles**
- **Equal-width bins**
- **Custom cut points**

For example, you can create an age group at 65 years:

``` r

dt[, age_cat := cut(age, breaks = c(-Inf, 65, Inf))]
```

The new variable can then be used like any other categorical variable.

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
