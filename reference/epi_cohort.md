# A synthetic clinical cohort

Six hundred simulated patients, shaped like the descriptive tables that
motivate this package: a few continuous measurements crossed with
several categorical variables worth stratifying on.

## Usage

``` r
epi_cohort
```

## Format

A data frame with 600 rows and 11 columns:

- id:

  Patient identifier, `"P0001"` to `"P0600"`.

- age:

  Age in years.

- sex:

  Factor: `"Male"`, `"Female"`.

- site:

  Factor: `"Site A"`, `"Site B"`, `"Site C"`, `"Site D"`. `"Site D"` has
  no observations.

- treatment:

  Factor: `"Control"`, `"Low dose"`, `"High dose"`.

- severity:

  Factor: `"Mild"`, `"Moderate"`, `"Severe"`.

- bmi:

  Body mass index, kg/m^2.

- crp:

  C-reactive protein, mg/L.

- los_days:

  Length of stay in days.

- fu_days:

  Days of follow-up, to death or to censoring. Censoring is by dropout
  or by the end of the study at 365 days.

- death:

  `1` if the patient died during follow-up, `0` if censored.

## Source

Simulated by `data-raw/epi_cohort.R`.

## Details

Two features are deliberate. `site` declares a fourth level, `"Site D"`,
that recruited nobody, so stratifying by `site` produces a stratum with
`n = 0`; the app lists it and draws no figure. `severity` has a small
`"Severe"` group, which the minimum-n rule filters out at its default
setting.

The data are simulated. They describe no real patients and support no
clinical conclusion.

`fu_days` and `death` make the data usable for a Kaplan-Meier curve, and
`age` for the categorization controls.

## Examples

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

# The empty stratum that the app reports with n = 0.
table(epi_cohort$site)
#> 
#> Site A Site B Site C Site D 
#>    296    173    131      0 
```
