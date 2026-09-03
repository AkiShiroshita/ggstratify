# Builds data/epi_cohort.rda. Run with:  source("data-raw/epi_cohort.R")
# Not shipped to CRAN: data-raw/ is listed in .Rbuildignore.

set.seed(20260812)
n <- 600L

sex <- factor(sample(c("Male", "Female"), n, TRUE, prob = c(0.48, 0.52)),
              levels = c("Male", "Female"))

# "Site D" is declared but recruited nobody. It is what makes this data set
# useful for demonstrating the n = 0 stratum.
site <- factor(sample(c("Site A", "Site B", "Site C"), n, TRUE,
                      prob = c(0.5, 0.3, 0.2)),
               levels = c("Site A", "Site B", "Site C", "Site D"))

treatment <- factor(sample(c("Control", "Low dose", "High dose"), n, TRUE),
                    levels = c("Control", "Low dose", "High dose"))

# A small stratum, to demonstrate the minimum-n rule.
severity <- factor(
  sample(c("Mild", "Moderate", "Severe"), n, TRUE, prob = c(0.6, 0.37, 0.03)),
  levels = c("Mild", "Moderate", "Severe")
)

age <- round(pmin(pmax(rnorm(n, 62, 14), 18), 95))
dose_effect <- c(Control = 0, `Low dose` = -1.4, `High dose` = -2.9)[treatment]

bmi <- round(rnorm(n, 24.5 + ifelse(sex == "Male", 0.9, 0), 3.8) +
               0.02 * (age - 62), 1)
crp <- round(exp(rnorm(n, 1.1 + as.numeric(severity) * 0.45, 0.8)), 1)
los <- round(pmax(rnorm(n, 8.5 + as.numeric(severity) * 2.2 + dose_effect, 3), 1))

# Follow-up, for the Kaplan-Meier curve. Death is more likely with greater
# severity and age, and less likely on treatment; follow-up is censored by
# dropout or by the end of the study at one year.
lp <- 0.9 * (as.numeric(severity) - 1) + 0.02 * (age - 62) +
  c(Control = 0, `Low dose` = -0.25, `High dose` = -0.55)[treatment]
t_event <- rexp(n, rate = 0.0016 * exp(lp))
t_cens <- pmin(rexp(n, rate = 0.0008), 365)

# CRP is not measured on everyone, and who it is measured on is not random:
# the milder the presentation, the more often it is left undone. That makes it
# missing at random given severity -- the pattern the "missing vs observed"
# layer exists to show, and one that a complete example data set cannot
# demonstrate.
#
# Drawn here, after every other variable, so that adding it leaves the rest of
# the data set exactly as it was: the draws above keep the stream positions
# they had when they were first generated.
p_unmeasured <- c(Mild = 0.28, Moderate = 0.10, Severe = 0.02)[severity]
crp[runif(n) < p_unmeasured] <- NA_real_

epi_cohort <- data.frame(
  id = sprintf("P%04d", seq_len(n)),
  age = age,
  sex = sex,
  site = site,
  treatment = treatment,
  severity = severity,
  bmi = bmi,
  crp = crp,
  los_days = los,
  fu_days = pmax(round(pmin(t_event, t_cens)), 1),
  death = as.integer(t_event <= t_cens),
  stringsAsFactors = FALSE
)

usethis::use_data(epi_cohort, overwrite = TRUE, compress = "xz")
