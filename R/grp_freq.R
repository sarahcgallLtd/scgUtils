#' @include utils.R
NULL
#' @title Grouped Frequencies in Survey Data
#' @name grp_freq
#'
#' @description
#' `grp_freq` calculates the frequency and optionally the percentage of a variable
#' by groups in survey data, supporting weighted and unweighted data.
#'
#' @param data A data frame containing survey data.
#' @param groups A vector of variable names for grouping. This parameter is required
#'   and can include one or multiple variables.
#' @param weight An optional variable containing weight factors for the analysis.
#' @param set_names An optional vector of custom column names for the output data frame.
#' @param addPercent Logical; if `TRUE`, adds a percentage column based on the frequency.
#'   The default is `FALSE`.
#' @param groupsPercent An optional vector of variables for calculating percentages.
#'   If `NULL`, percentages are calculated for the total.
#' @param round_decimals An optional numeric value specifying the number of decimal
#'   places to round numeric data. By default, no rounding is applied.
#'
#' @return A data frame containing frequencies and, optionally, percentages by group.
#'
#' @details
#' The function groups data based on the specified variables and calculates frequencies.
#' It can also calculate percentages, either overall or grouped by additional variables.
#' The data can be optionally weighted, and the output can be customised with specific column names.
#'
#' @examples
#' \dontrun{
#'   # Calculate unweighted frequencies by group with percent added
#'   df <- grp_freq(dataset, groups = c("age_categories", "gender"), addPercent = TRUE)
#'
#'   # Calculate weighted frequencies by group with custom column names
#'   df <- grp_freq(dataset, groups = c("age_categories", "gender"),
#'                  weight = "wgtvar", set_names = c("Age", "Gender", "n"))
#' }
#'
#' @export
grp_freq <- function(data,
                     groups,
                     weight = NULL,
                     set_names = NULL,
                     addPercent = FALSE,
                     groupsPercent = NULL,
                     round_decimals = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               groups = groups,
               weight = weight,
               groupsPercent = groupsPercent)
  # ==============================================================#
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # make "group_by" a list
  grp <- list_group(data, groups)

  # Get Frequency
  tmp <- calculate_frequency(data, grp, weight)

  # Option: add percent
  tmp <- optionally_add_percent(tmp, addPercent, groupsPercent, round_decimals)

  # Set names
  tmp <- set_column_names(tmp, groups, set_names, "Freq")

  # ==============================================================#
  return(tmp)
}


#' Calculate Frequency for Grouped Data
#'
#' An internal helper function for `grp_freq` that calculates the frequency of
#' each group in the data, optionally using a weighting variable.
#'
#' @param data The data frame containing the variables.
#' @param grp A list specifying the groups for which frequencies are calculated.
#' @param weight An optional weighting variable to be used in the calculation.
#'
#' @return A data frame with frequencies calculated for each group.
#'
#' @details
#' The function calculates the count of occurrences for each group if `weight` is `NULL`,
#' or sums the weights for each group if `weight` is provided.
#'
#' @noRd
calculate_frequency <- function(data,
                                grp,
                                weight = NULL
) {
  if (is.null(weight)) {
    stats::aggregate(grp[[1]], by = grp, FUN = length)
  } else {
    stats::aggregate(data[, weight], by = grp, FUN = sum)
  }
}

#' Optionally Add Percentage Column to Data Frame
#'
#' An internal helper function for `grp_freq` that adds a percentage column
#' to the data frame based on frequency, optionally grouping by specified variables.
#'
#' @param data The data frame to which the percentage column will be added.
#' @param addPercent Logical indicating whether to add the percentage column.
#' @param groupsPercent The variables used for grouping when calculating percentages.
#' @param round_decimals The number of decimal places for rounding the percentages.
#'
#' @return A data frame possibly including a new percentage column.
#'
#' @details
#' If `addPercent` is `TRUE` or `groupsPercent` is not `NULL`, the function calculates
#' the percentage representation of each frequency. The percentages are calculated
#' either for the entire data frame or grouped by `groupsPercent`.
#'
#' @noRd
optionally_add_percent <- function(data,
                                   addPercent = FALSE,
                                   groupsPercent = NULL,
                                   round_decimals = NULL
) {
  if (addPercent == TRUE || !is.null(groupsPercent)) {
    if (is.null(groupsPercent)) {
      data <- transform(data, Perc = stats::ave(x, FUN = percent))
    } else {
      data <- transform(data, Perc = stats::ave(x, data[, groupsPercent], FUN = percent))
    }

    data <- round_vars(data, round_decimals)
  }
  return(data)
}