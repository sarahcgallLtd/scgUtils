#' @include utils.R
NULL
#' @title Compile Multiple Data Frames into One
#' @name compile
#'
#' @description
#' The `compile` function generates a comprehensive CSV or statistics data frame
#' from survey data, combining crosstabs and/or statistics for each pair of row and column variables.
#'
#' @param data A data frame containing survey data.
#' @param rowVars A vector of names of independent variables, each represented in rows.
#' @param colVars A vector of names of dependent variables, each represented in columns.
#' @param weight Optional variable containing weight factors for the analysis.
#' @param format The desired output format: 'csv' for a formatted CSV file, or 'statistics'
#'   for a statistical summary (default is 'csv').
#' @param save Logical; if `TRUE`, the results are saved to a CSV file (default is `TRUE`).
#' @param name The name of the output file (without extension) when `save` is `TRUE`
#'   (default is 'table').
#'
#' @return If `save` is `FALSE`, returns a data frame containing the compiled results.
#'   If `save` is `TRUE`, the function saves the results to a CSV file and does not
#'   return a value.
#'
#' @examples
#' \dontrun{
#'   # Example: Compile crosstabs with CSV output using weighted data
#'   compile(data = dataset,
#'           rowVars = c("Q1", "Q2"),
#'           colVars = c("Gender", "VI"),
#'           weight = "wgtvar",
#'           name = "crosstabs")
#' }
#'
#' @importFrom magrittr %>%
#' @export
compile <- function(data,
                    rowVars,
                    colVars,
                    weight,
                    format = c("csv", "statistics"),
                    save = TRUE,
                    name = "table"
) {
  # ==============================================================#
  # CHECK PARAMS
  format <- match.arg(format)

  # ==============================================================#
  # CONSTRUCT DATA FRAME
  result <- data.frame()

  for (rowVar in rowVars) {
    tmp <- data.frame()
    for (colVar in colVars) {
      # Get crosstab
      df <- crosstab(data = data, rowVar = rowVar, colVar = colVar, weight = weight, format = format)

      # Process the crosstab result based on the format
      if (format == "csv") {
        df <- process_csv_format(df, data, rowVar, colVar)
      }
      tmp <- combine_colVar_results(tmp, df, format, rowVar)
    }
    # Once a full loop has completed for j, bind rows to result.
    result <- rbind(result, finalise_rowVar_results(tmp, format))
  }

  if (save) {
    save_results(result, format, name)
  } else {
    return(result)
  }
}

#' Process Data Frame for CSV Format
#'
#' This internal helper function for `compile` prepares a data frame for output
#' in CSV format by adding questions and formatting headers.
#'
#' @param df The data frame to process.
#' @param data The original data set used for obtaining questions.
#' @param rowVar The name of the row variable.
#' @param colVar The name of the column variable.
#'
#' @return A processed data frame with added row and column headers suitable for CSV output.
#'
#' @noRd
process_csv_format <- function(df, data, rowVar, colVar) {
  # Add a row of NAs followed by the column names and df
  df <- rbind(NA, colnames(df), df)

  # Get question of colVar and place in first row after rowVar and Total (ie 3rd column)
  df[1, 3] <- get_question(data, colVar)

  # Get question of rowVar and place in second row in the first column
  df[2, 1] <- get_question(data, rowVar)

  # Return df
  return(df)
}

#' Combine Column Variable Results
#'
#' An internal helper function for `compile` that combines results for different
#' column variables into a single data frame.
#'
#' @param tmp The temporary data frame to which new results are added.
#' @param df The data frame containing new results.
#' @param format The output format (e.g., 'csv').
#' @param rowVar The name of the row variable.
#'
#' @return An updated data frame combining the previous and new results.
#'
#' @noRd
combine_colVar_results <- function(tmp, df, format, rowVar) {
  # If loop is first time running, copy full dataset
  if (nrow(tmp) == 0) {
    tmp <- df
    # Otherwise, remove duplicated Total column from additional data frames and bind datasets by columns
  } else if (format == "csv") {
    tmp <- dplyr::full_join(tmp, subset(df, select = -c(Total)), by = c(rowVar))
    # Format for "statistics"
  } else {
    tmp <- rbind(tmp, df)
  }
  return(tmp)
}

#' Finalise Row Variable Results
#'
#' This internal helper function for `compile` finalizes the results for a row variable,
#' adjusting the format as needed for CSV output.
#'
#' @param tmp The temporary data frame containing the results to finalise.
#' @param format The desired output format.
#'
#' @return A finalised data frame ready for output.
#'
#' @noRd
finalise_rowVar_results <- function(tmp, format) {
  if (format == "csv") {
    # Reset column names
    colnames(tmp) <- paste("Column", 1:ncol(tmp), sep = " ")

    # Add blank row below question to divide questions
    tmp <- rbind(tmp, NA)
  }
  return(tmp)
}

#' Save Results to a File
#'
#' An internal helper function for `compile` that saves the final results to a CSV file.
#'
#' @param result The data frame containing the results to be saved.
#' @param format The format of the results, which determines header inclusion.
#' @param name The name of the output file (without extension).
#'
#' @return The function does not return a value, but writes a file to disk.
#'
#' @noRd
save_results <- function(result, format, name) {
  # If "statistics", make col.names = TRUE, otherwise FALSE
  col.names <- (format == "statistics")

  # Format csv
  utils::write.table(result,
                     file = paste0(name, ".csv"),
                     na = "",
                     row.names = FALSE,
                     col.names = col.names,
                     fileEncoding = "UTF-8",
                     sep = ',')
}