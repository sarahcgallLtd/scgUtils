#' @title Remove Unused Levels from Factors
#' @name remove_levels
#'
#' @description Remove unused levels from factors across a complete data frame. NB non-factor columns will be moved
#' to the beginning of the data frame.
#'
#' @param data A data frame containing survey data. This parameter is required.
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' df <- remove_levels(df)
#' }
#' @export
remove_levels <- function(data) {
  # Store attributes
  attributes_to_preserve <- get_factor_labels(data)

  # Identify factor columns
  factor_cols <- sapply(data, is.factor)

  # Subset the data to include only factor columns
  df_factors <- data[, factor_cols, drop = FALSE]

  # Re-factor all levels of data to remove unused levels
  df_factors[] <- lapply(df_factors, function(x) if (is.factor(x)) factor(x) else x)

  # Identify non-factor columns
  non_factor_cols <- !factor_cols

  # Include non-factor columns from the original data
  df <- cbind(data[, non_factor_cols, drop = FALSE], df_factors)

  # Restore original attributes
  df <- add_attributes(df, attributes_to_preserve)

  # Return the modified data
  return(df)
}
