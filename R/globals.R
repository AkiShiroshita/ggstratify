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
#' @importFrom stats setNames
#' @importFrom survival Surv survfit
#' @importFrom utils head modifyList
"_PACKAGE"

# Columns referenced by data.table's non-standard evaluation.
utils::globalVariables(c(
  ".", ".N", ".SD", ".facet_label", ".gs_stratum", ".strat_label",
  "can_stratify", "file", "is_categorical",
  "is_continuous", "is_event", "is_numeric", "keep", "label", "level", "n",
  "n_levels", "n_missing", "status", "var"
))
