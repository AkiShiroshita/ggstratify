# ggstratify: Fast Stratified Descriptive Figures with a Point-and-Click GUI

A point-and-click 'shiny' interface for the descriptive analysis that
comes before any model is chosen. Pass a data frame, pick the variable
to describe, and add the layers you want to see it within: a second
variable becomes the panels of a 'ggplot2' facet_wrap(), and further
variables become separate figures, one file each, taken either one
variable at a time or crossed. Every stratum is reported with the number
of observations behind it, on the figure and on each of its panels;
strata that contain none are listed rather than dropped, and rows with a
missing value in a layer variable are excluded and counted. A continuous
variable can be categorized into quantile groups, equal-width bins or
user-supplied cut points, and a variable of any type can be turned into
whether it is missing or observed, so that the rows a layer would
exclude become a stratum of their own; either can then be used as a
layer. The figure types follow those offered by the 'ggplotgui' package
and add the line plot for change over time, an optional LOWESS smoother,
and the Kaplan-Meier curve estimated by 'survival', with an optional
number-at-risk table. Columns are described as they are typed, so
convert each to the type you mean first. Figures are written as PNG or
SVG, and the application prints the 'ggplot2' code behind the figure on
screen, so that a description can be repeated, shared or accounted for
later. Everything runs locally, with no network access and no AI
involved.

## See also

Useful links:

- <https://github.com/AkiShiroshita/ggstratify>

- <https://akishiroshita.github.io/ggstratify/>

- Report bugs at <https://github.com/AkiShiroshita/ggstratify/issues>

## Author

**Maintainer**: Akihiro Shiroshita <akihirokun8@gmail.com> \[copyright
holder\]

Authors:

- Akihiro Shiroshita <akihirokun8@gmail.com> \[copyright holder\]

- Yuki Kataoka <youkiti@gmail.com>
