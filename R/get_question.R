#' @title Retrieve Original Question Label
#' @name get_question
#'
#' @description
#' This helper function retrieves the original question label associated with a
#' variable in a data frame. If no label attribute is present, it returns the
#' variable name itself.
#'
#' @param data A data frame containing the variable for which the question label
#'   is to be retrieved.
#' @param var The name of the variable within the data frame whose question label
#'   is being sought. This should be a string corresponding to the column name in
#'   `data`.
#'
#' @details
#' The function checks for a `label` attribute on the specified variable in the
#' data frame. If the `label` attribute exists, it is returned as the question
#' label; otherwise, the name of the variable (`var`) is returned.
#'
#' @return Returns the original question label as a string. If no label is
#'   associated with the variable, the variable name is returned.
#'
#' @note This function is used by `compile`, `crosstab`, and `plot_popn`.
#'
#' @export
get_question <- function(data, var) {
  question <- if (!is.null(attr(data[[var]], "label"))) attr(data[[var]], "label") else var
  return(question)
}