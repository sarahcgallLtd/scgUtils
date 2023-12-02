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
#' df <- grp_mean(dataset,
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
grp_mean <- function(data, var, group, weight, set_names) {
  grp <- list(group=data[,group])

  if (missing(weight)) {
    df <- stats::aggregate(data[,var],
                    by=grp$group,
                    FUN=mean)
  } else {
    df <- by(data[c(var, weight)],
             grp$group,
             FUN = function(x) stats::weighted.mean(x[,1], x[,2]))
    df <- data.frame(Col1 = rep(names(df), lengths(df)),
                     Col2 = c(unlist(df[[1]]),unlist(df[[2]])))
  }

  if (!missing(set_names)) {
    df <- stats::setNames(df, set_names)
  } else {
    df <- stats::setNames(df, c(group, "Mean"))
  }

  return(df)
}