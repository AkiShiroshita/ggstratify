#' A synthetic clinical cohort
#'
#' Six hundred simulated patients, shaped like the descriptive tables that
#' motivate this package: a few continuous measurements crossed with several
#' categorical variables worth stratifying on.
#'
#' Three features are deliberate. `site` declares a fourth level, `"Site D"`,
#' that recruited nobody, so stratifying by `site` produces a stratum with
#' `n = 0`; the app lists it and draws no figure. `severity` has a small
#' `"Severe"` group -- 20 patients against 381 and 199 -- which is what the
#' minimum-n control is there to be tried on: it is drawn at the default
#' minimum of 10, and disappears from the figures, with a reason, as soon as
#' the minimum is raised past 20. `crp` is the only column with missing
#' values, and they are not missing at random: the milder the presentation,
#' the more often the measurement was left undone, so 105 of the 381 mild
#' patients have no CRP against none of the 20 severe ones. Deriving *missing
#' vs observed* from `crp` and describing another variable within it is what
#' that pattern is there for.
#'
#' The data are simulated. They describe no real patients and support no
#' clinical conclusion.
#'
#' `admit_date` is the fourth of these. It spans three calendar years and has
#' several hundred distinct values, so it cannot be stratified on as it stands
#' -- that is the point of it. Read at a resolution first, by season or by
#' month of the year, it becomes a variable with four or twelve levels that can
#' be used as a layer like any other. The seasonality is in the case mix rather
#' than in the number of admissions: moderate and severe presentations cluster
#' in the winter and mild ones spread into the summer, so a season carries a
#' real difference in `crp` and in `los_days` rather than noise.
#'
#' `fu_days` and `death` make the data usable for a Kaplan-Meier curve, `age`
#' for the methods that cut a continuous variable into groups, `crp` for the
#' one that splits a variable by whether it has a value, and `admit_date` for
#' the one that reads a date at a chosen resolution.
#'
#' @format A data frame with 600 rows and 12 columns:
#' \describe{
#'   \item{id}{Patient identifier, `"P0001"` to `"P0600"`.}
#'   \item{age}{Age in years.}
#'   \item{sex}{Factor: `"Male"`, `"Female"`.}
#'   \item{site}{Factor: `"Site A"`, `"Site B"`, `"Site C"`, `"Site D"`.
#'     `"Site D"` has no observations.}
#'   \item{treatment}{Factor: `"Control"`, `"Low dose"`, `"High dose"`.}
#'   \item{severity}{Factor: `"Mild"`, `"Moderate"`, `"Severe"`.}
#'   \item{bmi}{Body mass index, kg/m^2.}
#'   \item{crp}{C-reactive protein, mg/L. `NA` for 122 patients who were not
#'     tested, more often the milder ones.}
#'   \item{los_days}{Length of stay in days.}
#'   \item{fu_days}{Days of follow-up, to death or to censoring. Censoring is
#'     by dropout or by the end of the study at 365 days.}
#'   \item{death}{`1` if the patient died during follow-up, `0` if censored.}
#'   \item{admit_date}{Date of admission, over 2021 to 2023. Moderate and
#'     severe cases cluster in the winter, mild ones in the summer.}
#' }
#'
#' @source Simulated by `data-raw/epi_cohort.R`.
#'
#' @examples
#' str(epi_cohort)
#'
#' # The empty stratum that the app reports with n = 0.
#' table(epi_cohort$site)
#'
#' # Who was not tested, by how ill they were.
#' table(epi_cohort$severity, is.na(epi_cohort$crp))
#'
#' # Too many distinct dates to stratify on, until it is read at a resolution.
#' length(unique(epi_cohort$admit_date))
#' table(factor(month.abb[data.table::month(epi_cohort$admit_date)],
#'              levels = month.abb))
"epi_cohort"
