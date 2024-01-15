#' @title Grouped Mean
#' @name grp_mean
#'
#' @description Calculates the mean of a variable by a group in survey data.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param meanVar Variable to be averaged. var must be numeric. This parameter is required.
#' @param groups Variable being grouped. This parameter is required and permits one or many variables.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param set_names Vector of column names. This paramenter is optional.
#' @param round_decimals Numeric value to round numeric data by x number of decimals places. Default does not round.
#'
#' @return A data frame containing averages by group
#'
#' @examples
#' \dontrun{
#' # Return a averages of a variable by group (weighted or unweighted)
#' tmp <- grp_mean(dataset,
#'                var = "age",
#'                groups = "gender",
#'                weight = "wgtvar")
#'
#' #   gender     Mean
#' # 1   Male 42.78670
#' # 2 Female 41.06441
#'
#' # NB for non-grouped averages, use mean(var) or weighted.mean(var, weight)
#' }
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
  # PREPARE DATA
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # make "group_by" a list
  grp <- list_group(data, groups)

  # ==============================================================#
  # CALCULATE MEAN
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
                      Col2 =as.numeric(tmp))

    # Split interaction group back into it's original group columns (NB this is done by searching for a full stop.
    # If the values have a full stop in them, this will cause a problem.
    tmp <- cbind(do.call("rbind", strsplit(tmp$Col1, split = "\\.")), tmp)
    # TODO: find alternative so that values are not affected by strsplitsplit(, "\\.")

    # Remove interaction group column
    tmp[["Col1"]] <- NULL
  }

  # ==============================================================#
  # OPTIONAL: ROUND NUMERIC VALUES
  if (is.numeric(round_decimals) == TRUE)
    tmp <- round_vars(tmp, round_decimals)

  # ==============================================================#
  # SET COLUMN NAMES
  if (!missing(set_names)) {
    tmp <- stats::setNames(tmp, set_names)
  } else {
    tmp <- stats::setNames(tmp, c(groups, "Mean"))
  }

  # ==============================================================#
  return(tmp)
}