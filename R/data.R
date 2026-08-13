#' A synthetic clinical cohort
#'
#' Six hundred simulated patients, shaped like the descriptive tables that
#' motivate this package: a few continuous measurements crossed with several
#' categorical variables worth stratifying on.
#'
#' Two features are deliberate. `site` declares a fourth level, `"Site D"`,
#' that recruited nobody, so stratifying by `site` produces a stratum with
#' `n = 0`; the app lists it and draws no figure. `severity` has a small
#' `"Severe"` group, which the minimum-n rule filters out at its default
#' setting.
#'
#' The data are simulated. They describe no real patients and support no
#' clinical conclusion.
#'
#' `fu_days` and `death` make the data usable for a Kaplan-Meier curve, and
#' `age` for the categorization controls.
#'
#' @format A data frame with 600 rows and 11 columns:
#' \describe{
#'   \item{id}{Patient identifier, `"P0001"` to `"P0600"`.}
#'   \item{age}{Age in years.}
#'   \item{sex}{Factor: `"Male"`, `"Female"`.}
#'   \item{site}{Factor: `"Site A"`, `"Site B"`, `"Site C"`, `"Site D"`.
#'     `"Site D"` has no observations.}
#'   \item{treatment}{Factor: `"Control"`, `"Low dose"`, `"High dose"`.}
#'   \item{severity}{Factor: `"Mild"`, `"Moderate"`, `"Severe"`.}
#'   \item{bmi}{Body mass index, kg/m^2.}
#'   \item{crp}{C-reactive protein, mg/L.}
#'   \item{los_days}{Length of stay in days.}
#'   \item{fu_days}{Days of follow-up, to death or to censoring. Censoring is
#'     by dropout or by the end of the study at 365 days.}
#'   \item{death}{`1` if the patient died during follow-up, `0` if censored.}
#' }
#'
#' @source Simulated by `data-raw/epi_cohort.R`.
#'
#' @examples
#' str(epi_cohort)
#'
#' # The empty stratum that the app reports with n = 0.
#' table(epi_cohort$site)
"epi_cohort"
