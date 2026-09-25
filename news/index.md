# Changelog

## ggstratify 0.2.0

CRAN release: 2026-09-25

- The dots of a **Dot + Error** figure can be joined by a line, under
  **Plot options**. A set of estimates along a time axis is usually read
  for which way it went, and the line is what says so. It is computed by
  the same summary as the points it joins – the same helper at the same
  confidence level – so it passes through them rather than near them,
  and it is drawn beneath them. One line per colour when there is a
  grouping variable, and per panel.

- **Survey weights.** A numeric column can be set as the survey weight
  under **Describe**. Each row then counts for as many people as its
  weight: histograms, densities, boxplots and violins are drawn through
  `ggplot2`’s `weight` aesthetic, and a LOWESS smoother is fitted with
  the weights. A dotplot draws one dot per row and cannot be weighted,
  so it is refused with the reason rather than drawn unweighted.

- A weighted figure reports two counts wherever it reports one – in the
  title, on every panel strip, in the **Strata** tab and in the
  number-at-risk table: `N`, the rows the figure is drawn from, and the
  weighted N, the sum of their weights. They answer different questions,
  and neither can be read off the other. The minimum N per figure is
  still a number of rows.

- The bar on a weighted **Dot + Error** figure and the band on a
  weighted Kaplan-Meier curve are design-based, estimated with the
  ‘survey’ package, which is now a dependency. The design takes the
  weight and, optionally, the **sampling strata** and the **clusters**
  (primary sampling units), with a cluster ID read within its stratum
  (`nest = TRUE`). A mean gets its standard error or its t interval; a
  proportion gets `svyciprop()`’s Korn-Graubard interval (in place of
  Clopper-Pearson) or its Wilson interval. The curve is `svykm()`, and
  its band is drawn on the log scale as
  [`confint()`](https://rdrr.io/r/stats/confint.html) draws it.

- The design is built once, over the whole sample, before any row is set
  aside for a layer, and every panel and figure is a subpopulation of
  it. Its standard errors therefore count every stratum and cluster in
  the sample – including the clusters it holds none of – rather than
  those of a design rebuilt on its own rows, and its intervals are read
  on the whole design’s degrees of freedom. The generated script builds
  the design in the same place.

- A stratum that holds a single cluster has no variance to estimate, and
  a figure that needs one is refused with the stratum named and a
  suggestion to merge it, rather than failing inside ‘survey’. A row
  with no stratum or cluster is excluded and counted, as a row with no
  weight is.

- `epi_cohort` gains `svy_weight`, a survey weight that reads the cohort
  as a sample of about 50,000 admissions with the severe presentations
  over-sampled. The other twelve columns are unchanged.

- A smoother’s band is not offered while a weight is set.
  [`loess()`](https://rdrr.io/r/stats/loess.html) reads a weight as the
  precision of an observation, not as the number of people it stands
  for, so the band would not be a design-based interval.

- A row with no weight is excluded and counted, in the same place and
  the same way as a row with no value for a layer variable.

- **Derive a variable** gains a fifth method: *time resolution*. A date
  or a date-time is read at whatever resolution the question is asked
  at, and the answer is a variable that can be used as a layer like any
  other. A column of admission dates has as many values as there are
  days and cannot be stratified on at all; read by season it has four.

- The resolutions come in two families, because they answer different
  questions. A **calendar period** – year, quarter, month, week, day,
  hour, minute – keeps the year, so every row in March 2021 lands
  together and the values still run along real time. That matters for a
  trend: the column stays a date, so a figure drawn against it is spaced
  by elapsed time rather than by level number, and its axis can still be
  given date ticks. A **position in the cycle** – month of the year,
  season, quarter of the year, day of the week, hour of the day – drops
  the year and pools every March since the data began, which is what a
  seasonal pattern is.

- The months of the year read January to December and the days of the
  week Monday to Sunday. Sorting them would put April first and Friday
  second, which is never what was meant, so the levels are written out
  rather than derived.

- A season’s start month is chosen rather than assumed. The four seasons
  follow it three months at a time, so March gives the northern
  hemisphere and September the southern; the order spring, summer, fall,
  winter does not move with it.

- A resolution a column cannot answer is refused with the reason, not
  with the nearest available message. An hour of the day taken from a
  plain date would be midnight for every row – the underlying function
  answers rather than refusing – and a month taken from a time of day
  has no calendar to fall in.

- A `Line` plot drawn against a date or a date-time can say how far
  apart its ticks are and what each one reads. The values plotted do not
  change; only the ticks over them, so a column that really holds dates
  can be shown as a run of years. With no axis label typed, the axis is
  titled by the unit – `Year` rather than the column’s name – since that
  is what the ticks now say.

- A number typed into **X axis from** or **X axis to** while the X
  variable is a date no longer stops the figure. It could not have
  worked – those boxes are numeric and hold no date – but ‘ggplot2’ does
  not ignore a number handed to a date scale, it refuses to draw at all.
  The range is now dropped and the figure appears.

- A time-of-day column stored as
  [`data.table::ITime`](https://rdrr.io/pkg/data.table/man/IDateTime.html)
  is no longer offered as a continuous measurement or as a Kaplan-Meier
  follow-up time. It is a count of seconds past midnight and so reads as
  a number, which is not a reason to plot a clock on an axis of
  measurements.

- The name suggested for a derived variable now says which resolution it
  is (`admit_date_season`, not `admit_date_cat`), and the box is
  refreshed only when it still holds a name this application put there.
  It used to be refreshed whenever the name merely looked like one,
  which a real column called `birth_year` or `treatment_cat` does.

- The bar on a **Dot + Error** figure can say what it stands for. It has
  always been one standard error either side of the mean, which
  describes how precisely the mean is known and is about half the width
  a reader takes an error bar to have. A confidence interval is now
  offered beside it, at 90, 95 or 99 percent.

- The mean’s interval uses the t distribution, because the standard
  error is itself estimated from the same observations. It is the
  interval [`t.test()`](https://rdrr.io/r/stats/t.test.html) reports and
  the one `Hmisc::smean.cl.normal()` computes, written out in the
  generated script so that running that script needs no further package.

- A Y variable coded 0 and 1 is a proportion rather than a measurement,
  and it gets its own two intervals: **exact** (Clopper-Pearson), which
  inverts the binomial test and so never covers less often than it
  claims, and **Wilson**, which inverts the score test and is the
  better-centred of the two at small

  14. Neither runs outside 0 and 1, and neither collapses to nothing in
      a stratum where no one had the outcome – which is where a mean and
      a standard error are at their most misleading and where a
      descriptive figure most needs to be honest.

- Which of the two to prefer is a judgement rather than a fact, so both
  are offered and neither is the default.

- A 0/1 variable is now offered as the Y variable of a Dot + Error
  figure. It was not offered at all before – having two values is
  exactly what rules a column out as a measurement – so the one figure
  that can describe a proportion was the one figure that could not be
  pointed at one.

- The error bars offered follow the Y variable that is chosen. A
  measurement is offered the standard error and the mean’s interval; a
  0/1 outcome is offered the two proportion intervals and nothing else.
  Neither set applies to the other, so the list is narrowed rather than
  shown in full and half of it refused afterwards.

- An outcome is accepted however it is written: 0 and 1, `TRUE` and
  `FALSE`, a two-level factor, or two character values. Most people have
  already made theirs a factor or a logical, so requiring 0 and 1 would
  have meant recoding a column to get at a figure describing it.

- Which of the two values counts as the outcome having happened is a
  control rather than an assumption. It defaults to the second – the
  reference level first, the level being modelled second, as
  [`glm()`](https://rdrr.io/r/stats/glm.html) reads a factor – because a
  factor declared `c("Yes", "No")` means the opposite of one declared
  the other way round and nothing in the data says which was intended.

- The counting is written into the figure, as
  `y = as.integer(outcome == "Died")`, rather than done to the data
  beforehand. ‘ggplot2’ would otherwise put a non-numeric Y on a
  discrete scale and hand the interval the level number – 1 and 2 –
  which comes back as a proportion above one with a `NaN` as the only
  sign that anything is wrong. Doing it in the
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) keeps the
  column as it was typed and puts what was counted on the screen; the Y
  axis is labelled `Proportion outcome = Died` to match.

- A declared factor level that nobody had still counts as one of the
  two, so a stratum where the outcome never happened is a proportion of
  0 with a real interval around it rather than a missing figure.

- Tick labels on either axis can be turned, by 30, 45, 60 or 90 degrees.
  Twelve month names along one axis do not fit side by side, and
  ‘ggplot2’ overlaps them rather than dropping any. The turn is emitted
  after the theme, since a theme replaces the element rather than
  merging into it.

- The bundled `epi_cohort` gains `admit_date`, over 2021 to 2023. The
  seasonality is in the case mix rather than in the number of
  admissions: moderate and severe presentations cluster in the winter
  and mild ones spread into the summer, so a season read off it carries
  a real difference in `crp` and in `los_days`. The other eleven columns
  are unchanged, values included.

## ggstratify 0.1.0

- A numeric variable is now treated as continuous because of its type,
  not because it has enough distinct values. Counting distinct values
  guessed wrong on measurements that are recorded coarsely – an age in
  whole years over a narrow range, a visit number, a score out of five –
  and refused to plot them on an axis. A column holding at most two
  values is still not offered as a measurement: an indicator coded 0/1
  cannot describe a spread. A few-levelled numeric column can now be
  both, plotted on an axis and stratified by.

- A line plot’s **Mark the observations** option has moved from **Plot
  options** to **Describe**, beside the rest of the line settings, and
  draws its points at `size = 1` so that a dense set of them does not
  cover the line it belongs to.

- **Derive a variable** (the panel formerly called Categorize) gains a
  fourth method: *missing vs observed*. It reads no values, only whether
  there is a value, so it applies to a variable of any type – a factor,
  a character column or a date as much as a number – and turns it into
  two groups that can be used as a layer like any other categorical
  variable.

- Rows with a missing value in a layer variable are excluded, as before.
  A missing-vs-observed variable is the one layer that never has one:
  [`is.na()`](https://rdrr.io/r/base/NA.html) answers for every row. The
  rows another layer would have dropped are therefore exactly the ones
  this puts in front of you, which is what makes it a way to look at who
  is not in the data rather than only to count them.

- Describing a variable within its own missingness leaves the Missing
  figure with nothing to draw. The app reports that arrangement under
  **Layers** instead of drawing an empty figure without comment, and
  still draws it.

- A variable with no missing values, or with nothing but missing values,
  gives one group rather than two and is refused with a note saying
  which.

## ggstratify 0.0.1

CRAN release: 2026-09-02

- Initial release.

- [`ggstratify()`](https://akishiroshita.github.io/ggstratify/reference/ggstratify.md)
  takes a data frame or a matrix that is already in your session, and
  nothing else. There is no file-upload control in the application and a
  file path is no longer accepted as `dataset`. Data read from disk is
  typed by whatever the reader guessed, and would then be described on
  those guesses; read the file yourself, look at the result, and pass
  the object.

- The name the data was passed under is what the generated code refers
  to it by, so `ggstratify(cohort)` prints code that starts
  `d <- cohort`.

- Previews are subsampled only above one million rows for scatter and
  line plots and ten million for the plot types that summarise their
  rows first, and never for Kaplan-Meier curves.
