#' @keywords internal
#' @import data.table
#' @import ggplot2
#' @import shiny
#' @importFrom grDevices svg
# patchwork is called from generated code -- `patchwork::wrap_plots()` stacks
# the number-at-risk table under its curve -- which the package then evaluates.
# That is a real run-time dependency, but it lives in a string, where R CMD
# check cannot see it; naming one function here is what makes it visible.
#' @importFrom patchwork wrap_plots
# survey is in the same position: a weighted Dot + Error figure and a weighted
# Kaplan-Meier curve are estimated by `survey::` calls inside generated code.
#' @importFrom survey svydesign
#' @importFrom stats setNames
#' @importFrom survival Surv survfit
#' @importFrom utils head modifyList
"_PACKAGE"

# Columns referenced by data.table's non-standard evaluation.
utils::globalVariables(c(
  ".", ".I", ".N", ".SD", ".facet_label", ".gs_stratum", ".strat_label",
  ".svy_row",
  "can_stratify", "file", "is_binary", "is_categorical", "is_continuous",
  "is_event", "is_numeric", "is_temporal", "is_weight", "keep", "label",
  "level", "n", "n_levels", "n_missing", "n_w", "status", "var"
))
