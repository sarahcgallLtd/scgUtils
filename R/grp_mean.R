#' @include utils.R
NULL
#' @title Calculate Grouped Mean in Survey Data
#' @name grp_mean
#'
#' @description
#' `grp_mean` calculates the mean (either unweighted or weighted) of a specified variable,
#' grouped by one or more variables in survey data.
#'
#' @param data A data frame containing survey data.
#' @param meanVar The variable for which the mean is to be calculated; must be numeric.
#' @param groups Variables used for grouping; one or many variables can be specified.
#' @param weight An optional variable containing weight factors for calculating the weighted mean.
#' @param set_names An optional vector of custom column names for the output data frame.
#' @param round_decimals An optional numeric value specifying the number of decimal
#'   places for rounding the mean values.
#'
#' @return A data frame containing the calculated means for each group, with the option
#'   to include weighted means and custom column names.
#'
#' @details
#' The function can calculate either an unweighted mean if `weight` is `NULL`, or a
#' weighted mean if `weight` is provided.
#' If `weight` is provided, it calculates a weighted mean, accounting for the interaction
#' between group variables. The function is designed to work with group interactions
#' split by a full stop (.), which might affect the handling of group values containing periods.
#' The results can be rounded to a specified
#' number of decimal places, and custom column names can be set for the output data frame.
#'
#' @examples
#' \dontrun{
#'   # Calculate unweighted mean of 'age' grouped by 'gender'
#'   df <- grp_mean(dataset, meanVar = "age", groups = "gender")
#'
#'   # Calculate weighted mean of 'age' grouped by 'gender', with custom column names
#'   df <- grp_mean(dataset, meanVar = "age", groups = "gender",
#'                  weight = "wgtvar", set_names = c("Gender", "Average Age"))
#' }
#'
#' @export
grp_mean <- function(data,
                     meanVar,
                     groups,
                     weight = NULL,
                     set_names = NULL,
                     round_decimals = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               meanVar = meanVar,
               groups = groups,
               weight = weight)

  # ==============================================================#
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # make "group_by" a list
  grp <- list_group(data, groups)

  # Calculate mean
  tmp <- calculate_mean(data, meanVar, groups, grp, weight)

  # Option: Round decimals
  tmp <- round_vars(tmp, round_decimals)

  # Option: Set names
  tmp <- set_column_names(tmp, groups, set_names, "Mean")

  # ==============================================================#
  return(tmp)
}

#' Calculate Mean for Grouped Data
#'
#' This internal helper function for `grp_mean` calculates the mean (either unweighted or weighted)
#' of a specified variable, grouped by given categories.
#'
#' @param data The data frame containing the variables.
#' @param meanVar The name of the variable for which the mean is to be calculated.
#' @param groups A vector of variable names used for grouping.
#' @param grp A list specifying the groups for mean calculation.
#' @param weight An optional weighting variable for calculating the weighted mean.
#'
#' @return A data frame with mean values calculated for each group. For weighted
#'   means, it also handles the interaction of group variables.
#'
#' @details
#' If `weight` is `NULL`, the function calculates an unweighted mean for each group.
#' If `weight` is provided, it calculates a weighted mean, accounting for the interaction
#' between group variables. The function is designed to work with group interactions
#' split by a full stop (.), which might affect the handling of group values containing periods.
#'
#' @noRd
calculate_mean <- function(data,
                           meanVar,
                           groups,
                           grp,
                           weight
) {
  if (is.null(weight)) {
    # Unweighted Mean
    tmp <- stats::aggregate(data[, meanVar],
                            by = grp,
                            FUN = mean,
                            na.rm = TRUE
    )
  } else {
    # Weighted Mean
    # Create interaction group and add to data frame
    data$interaction_group <- interaction(data[groups])

    # Get weighted mean by interaction group
    tmp <- by(data[c(meanVar, weight)],
              data$interaction_group,
              FUN = function(x) stats::weighted.mean(x[[1]], x[[2]]))

    # Convert matrix into data frame
    tmp <- data.frame(Col1 = names(tmp),
                      Col2 = as.numeric(tmp))

    # Split interaction group back into it's original group columns (NB this is done by searching for a full stop.
    # If the values have a full stop in them, this will cause a problem.
    tmp <- cbind(do.call("rbind", strsplit(tmp$Col1, split = "\\.")), tmp)
    # TODO: find alternative so that values are not affected by strsplitsplit(, "\\.")

    # Remove interaction group column
    tmp[["Col1"]] <- NULL
  }
  return(tmp)
}

