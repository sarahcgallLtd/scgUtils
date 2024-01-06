#' Get Dataset
#'
#' `get_data()` is a helper function for loading datasets. It provides the user with the ability to set
#' the name of the data frame within their own environment (e.g. df).
#'
#' @param ... A string, naming the desired dataset.
#'   Available datasets include:
#'   * `survey`: Wave 25 of the 2014-2023 British Election Study Internet Panel
#' @returns
#'   `get_data()` returns a data frame.
#'
#' @examples
#' df <- get_data("survey")
#' @export
get_data <- function(...) {
    e <- new.env()
    name <- utils::data(..., envir = e)[1]
    e[[name]]
}
