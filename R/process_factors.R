#' @title Remove Unused Levels from Factors
#' @name process_factors
#'
#' @description Removes unused levels from factor variables in a data frame while preserving non-factor columns.
#' Non-factor columns are retained at the beginning of the data frame.
#'
#' @param data A data frame containing survey data.
#'
#' @return A modified data frame with unused levels removed from factor variables.
#'
#' @examples
#' \dontrun{
#' df <- process_factors(df)
#' }
#' @export
process_factors <- function(data
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data)

  if (!any(sapply(data, is.factor))) {
    warning("No factor columns found in the data frame.")
    return(data)
  }

  # ==============================================================#
  # PREPARE ATTRIBUTES
  # Store original row names
  original_row_names <- row.names(data)

  # Store attributes
  attributes_to_preserve <- get_factor_labels(data)

  # ==============================================================#
  # PROCESS DATA
  # Process factor columns
  data <- lapply(data, function(column) {
    if (is.factor(column)) {
      factor(column)
    } else {
      column
    }
  })

  # ==============================================================#
  # RETURN DATA FRAME
  # Convert list back to data frame / tibble / data.table preserving original type
  data <- data.frame(data, row.names = original_row_names, check.names = FALSE)

  # Restore original attributes
  data <- add_attributes(data, attributes_to_preserve)

  # Return the modified data
  return(data)
}

#' Retrieve "label" attribute for each factor column
#'
#' This is an internal helper function used by `process_factors` to get the "label" attribute
#' for factor columns in a data frame.
#'
#' @param data A data frame.
#'
#' @return A named list of "label" attributes for each factor column.
#' @noRd
get_factor_labels <- function(data) {
  # Extract "label" attributes for factor columns
  factor_labels <- vapply(data, function(column) {
    if (is.factor(column)) attr(column, "label") else ""
  }, FUN.VALUE = character(1))

  # Filter out the empty strings and return
  return(factor_labels[factor_labels != ""])
}

#' Add attributes to a data frame
#'
#' This is an internal helper function used by `process_factors` to add attributes back to a data frame.
#'
#' @param data A data frame.
#' @param attribute_labels A list of attributes to add.
#'
#' @return A data frame with added attributes.
#' @noRd
add_attributes <- function(data, attribute_labels) {
  # Iterate through attribute labels
  for (label in names(attribute_labels)) {
    # Check if the label exists in the data frame
    if (label %in% names(data)) {
      # Set the attribute in the new data frame
      attr(data[[label]], "label") <- attribute_labels[[label]]
    }
  }
  # Return data frame
  return(data)
}