# Describe a data set, one layer at a time

Launches a point-and-click Shiny interface for descriptive analysis:
pick the variable you want to describe, then add the layers you want to
see it within. The figure, the number of observations behind it and the
R code that reproduces it all appear together.

## Usage

``` r
ggstratify(dataset, launch.browser = TRUE, ...)
```

## Arguments

- dataset:

  The data to describe, and the one thing the function needs: a data
  frame – including a `tibble` or a `data.table` – or a matrix, already
  read into your session and already of the types you mean. A file path
  is not accepted; see *Before you start*. Whatever it is given becomes
  a plain `data.table`, so a grouped `tibble` is described as its rows,
  not as its groups. The object is copied, never modified in place, and
  its name is what the generated code refers to it by – so pass a named
  object, rather than an expression: `ggstratify(head(cohort))` is
  described perfectly well, but the code it prints says `d <- mydata`,
  because `head(cohort)` is not a name.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). `TRUE`
  opens the system browser.

- ...:

  Further arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly `NULL`, after the app is closed. Called for its side effect of
running a Shiny application.

## The layers

A description has a variable being described and, around it, the
variables you want to see it within. `ggstratify` calls those the
layers, and asks you to place each one:

- One layer:

  The variable on its own. A plain `ggplot2` figure.

- Two layers:

  The second variable becomes the panels of a `facet_wrap()`, so the
  whole comparison is one figure.

- Three or more:

  Two layers is what a single figure holds. Beyond that, you choose: one
  variable stays as the `facet_wrap()` panels, and the rest become
  separate figures – one file each. With columns A, B, C and D you might
  describe D, panel it by C, and get one figure per level of A and of B.

The variables that make separate figures can be treated separately,
which is the default – ticking `sex` and `treatment` gives the figures
`sex_M`, `sex_F`, `treatment_A`, ... – or crossed, giving one figure per
observed combination. Separately is usually the right question for
descriptive work: crossing two five-level variables gives twenty-five
mostly empty figures.

## What it tells you

Every level is reported with the number of observations it contains, and
that number is written into the figure title and onto each panel strip
inside it: a strip reads `site: Site A (N = 303)` rather than `Site A`,
so that a level which does not explain itself – the bare range a
categorized variable produces – still says which variable it is a level
of. A level with no observations at all (an unused factor level) is
listed with `N = 0` and produces no figure. Rows with a missing value in
any layer variable are excluded – a row that does not say which panel it
belongs to cannot be drawn in one – and the number excluded is reported
on the Strata tab and by the generated code.

## Three things descriptive work keeps needing

Under **Derive a variable**, a new categorical variable is made from one
you already have, and can then be used as a layer like any other
categorical variable. A continuous variable becomes quantile groups,
equal-width bins or your own cut points. Any variable, of any type,
becomes *missing vs observed*: the rows where it has a value and the
rows where it does not.

That last one is the layer for the question of who is not in the data.
Rows with a missing value in a layer variable are excluded, but a
missing-vs-observed variable never has one –
[`is.na()`](https://rdrr.io/r/base/NA.html) answers for every row – so
the rows that some other layer would have dropped are exactly the ones
it puts in front of you. Describe another variable within it to see how
the people whose CRP was never measured differ from the people whose CRP
was. Describing the variable itself within its own missingness leaves
the Missing figure with nothing to draw, and the app says so under
**Layers** rather than drawing an empty figure without comment.

Under **Type of graph**, *Kaplan-Meier curve* draws survival curves from
a time variable and an event indicator, fitted with
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
within each group, panel and figure, optionally with a confidence band,
censoring marks and a number-at-risk table under the curve. *Line* draws
change over time: put time on the X axis and the measurement on the Y
axis, and name the subject identifier under **One line per** to get one
line per subject.

A *Line* or *Scatter* figure can carry a LOWESS smoother –
[`stats::loess()`](https://rdrr.io/r/stats/loess.html) through
[`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
– with its span under your control and, when a grouping variable is set,
one fit per group.

## Before you start

Convert each column to the type you mean it to have – `numeric` for
measurements, `factor` for groups, with the levels in the order you want
them read – before passing the data. `ggstratify` describes what it is
given; it does not guess what you meant. A grouping variable stored as
`1, 2, 3` will be described as a number.

This is why the data must be an object you already have in your session.
There is no file to choose from inside the application, and no file path
to hand it: a data set that arrives by being read from disk arrives with
types that a reader guessed, and it is then described on those guessed
types without anyone having looked at them. Read the file yourself, run
[`str()`](https://rdrr.io/r/utils/str.html) or
[`summary()`](https://rdrr.io/r/base/summary.html) over the result, fix
the types that are wrong, and pass that object:

    cohort <- read.csv("cohort.csv")
    cohort$treatment <- factor(cohort$treatment,
                               levels = c("Control", "Low dose", "High dose"))
    str(cohort)
    ggstratify(cohort)

The figure types, themes and palettes otherwise match those offered by
the ggplotgui package. All data handling uses data.table. Figures are
written as PNG (through ragg) or as SVG, and the "R-code" tab shows the
self-contained ggplot2 code for the figure on screen – just the figure,
since writing the files is what the export button is for. The preview,
the export button and that code are all produced by the same code
generator, so the code you are shown is the code that made the figure.

## Examples

``` r
if (interactive()) {
  ggstratify(iris)
  ggstratify(epi_cohort)
}
```
