#' @include utils.R
NULL
#' @title Pivot Binary Grid Survey Questions Longer
#' @name grid_vars
#'
#' @description
#' The `grid_vars` function transforms binary grid survey questions into a longer format,
#' enabling comparisons across different variables or groups. It supports optional grouping
#' and weighting.
#'
#' @param data A data frame containing survey data.
#' @param vars A list mapping original column names to new variable names. This is
#'   required and allows multiple variables. For example, `list(Q1a = "Art", Q1b = "Automobiles")`.
#' @param group An optional variable for comparing across groups. Only one group can
#'   be specified.
#' @param weight An optional variable containing weight factors for the analysis.
#'
#' @return A data frame in a long format, with each row representing a response to a
#'   binary grid question. The data frame includes frequencies and optionally percentages
#'   if groups and/or weights are specified.
#'
#' @details
#' The function first converts specified binary grid questions into a long format using
#' `tidyr::pivot_longer`. It then uses `grp_freq` to calculate frequencies and, if
#' applicable, percentages for each question-response pair. This allows for an in-depth
#' analysis of binary grid survey questions, especially when combined with group and weight parameters.
#'
#' @examples
#' \dontrun{
#'   # Example: Convert grid questions to long format and analyze by gender
#'   vars <- list(Q1a = "Art", Q1b = "Automobiles", Q1c = "Birdwatching")
#'   df <- grid_vars(dataset, vars = vars, group = "gender", weight = "wgtvar")
#' }
#'
#' @export
grid_vars <- function(data,
                      vars,
                      group = NULL,
                      weight = NULL
) {
  # ==============================================================#
  # CHECK PARAMETERS
  check_params(data = data,
               vars = vars,
               group = group,
               weight = weight)

  # ==============================================================#
  # Prepare variables
  x <- names(vars)
  y <- append_if_exists(group, weight, x)

  # TRANSFORM DATA
  # Subset data
  tmp <- data[, y]

  # Ensure all character classes are converted to factors
  tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)

  # Make data frame longer
  tmp <- tidyr::pivot_longer(tmp, cols = names(tmp[, x]), names_to = "Question", values_to = "Response")

  # Change names of vars to from column names to new names
  tmp$Question <- unlist(vars)[tmp$Question]

  # Make Question variable factor
  tmp$Question <- factor(tmp$Question)

  # Get grouped frequency and percent
  tmp <- grp_freq(data = tmp,
                  groups = append_if_exists("Question", "Response", group),
                  weight = weight,
                  addPercent = TRUE,
                  groupsPercent = append_if_exists("Question", group),
                  round_decimals = 2)

  # ==============================================================#
  return(tmp)
}
