#' @title Grouped Mean
#' @name grp_mean
#'
#' @description Calculates the mean of a variable by a group in survey data.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param var Variable to be averaged. var must be numeric. This parameter is required.
#' @param group Variable being grouped. This parameter is required.
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
#'                group = "gender",
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
                     var,
                     group,
                     weight,
                     set_names,
                     round_decimals = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               var = var,
               group = group,
               weight = weight)

  stopifnot("`var` must be numeric." = is.numeric(data[[var]]))

  # ==============================================================#
  # PREPARE DATA
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # make "group_by" a list
  grp <- list_group(data, group)
#  grp <- list(group = data[, group])

  # ==============================================================#
  # CALCULATE MEAN
  if (missing(weight)) {
    tmp <- stats::aggregate(data[, var],
                            by = grp,
                            FUN = mean)
  } else {
    tmp <- by(data[c(var, weight)],
              grp,
              FUN = function(x) stats::weighted.mean(x[, 1], x[, 2]))
    tmp <- data.frame(Col1 = rep(names(tmp), lengths(tmp)),
                      Col2 = c(unlist(tmp[[1]]), unlist(tmp[[2]])))
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
    tmp <- stats::setNames(tmp, c(group, "Mean"))
  }

  # ==============================================================#
  return(tmp)
}