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
                     set_names
) {
  # ==============================================================#
  # PREPARE DATA
  grp <- list(group = data[, group])

  # ==============================================================#
  # CALCULATE MEAN
  if (missing(weight)) {
    tmp <- stats::aggregate(data[, var],
                           by = grp$group,
                           FUN = mean)
  } else {
    tmp <- by(data[c(var, weight)],
             grp$group,
             FUN = function(x) stats::weighted.mean(x[, 1], x[, 2]))
    tmp <- data.frame(Col1 = rep(names(tmp), lengths(tmp)),
                     Col2 = c(unlist(tmp[[1]]), unlist(tmp[[2]])))
  }

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