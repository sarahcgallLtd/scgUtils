#' @title Compile Multiple Data Frames in One
#' @name compile
#'
#' @description Produce a csv containing a full set of crosstabs or statistics for each 2x2 variables.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param rowVars Independent variable represented in rows (on the side). This parameter is required.
#' @param colVars Dependent variable represented in columns (at the top). This parameter is required.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param format Formatting options to return either a csv or statistics data frame (default = \code{"csv"}).
#' @param save Logical, if \code{TRUE}, df will be saved to .csv file (default = \code{TRUE}).
#' @param name Name of df to be saved to project directory (default = \code{"table"}).
#'
#' @return .csv file
#'
#' @examples
#' \dontrun{
#' # Create crosstabs with csv output using weighted data.
#' compile(dataset,
#'        rowVars = c("Q1", "Q2"),
#'        colVars = c("Gender", "VI"),
#'        weight = "wgtvar",
#'        name = "crosstabs")
#' }
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
  # CONSTRUCT FUNCTION ARGUMENTS
  xtab <- match.call(expand.dots = FALSE)
  xtab_n <- match(c("data", "weight", "format"), names(xtab), 0L)
  xtab <- xtab[c(1L, xtab_n)]
  xtab[[1L]] <- quote(crosstab)
  xtab[["format"]] <- match.arg(format)

  xtab[["statistics"]] <- FALSE
  xtab[["plot"]] <- FALSE
  # ==============================================================#
  # CONSTRUCT DATA FRAME
  # result <- list()
  result <- data.frame()
  for (i in rowVars) {
    xtab[["rowVar"]] <- i

    tmp <- data.frame()
    for (j in colVars) {
      xtab[["colVar"]] <- j

      # Evaluate: crosstab(data = data, weight = weight, format = format, statistics = FALSE, plot = FALSE)
      df <- eval(xtab, parent.frame())

      # Format for "csv"
      if (format == "csv") {
        # Add a row of NAs followed by the column names and df
        df <- rbind(NA, colnames(df), df)

        # Get question of colVar and place in first row after rowVar and Total (ie 3rd column)
        df[1, 3] <- get_question(data, j)

        # Get question of rowVar and place in second row in the first column
        df[2, 1] <- get_question(data, i)

        # If loop is first time running, copy full dataset
        if (nrow(tmp) == 0)
          tmp <- df

          # Otherwise, remove duplicated Total column from additional data frames and bind datasets by columns
        else
          tmp <- dplyr::full_join(tmp, subset(df, select = -c(Total)), by = c(i))

          # Format for "statistics"
      } else
        tmp <- rbind(tmp, df)
    }
    # Format for "csv"
    if (format == "csv") {
      # Reset column names
      colnames(tmp) <- paste("Column", 1:length(tmp), sep = " ")

      # Add blank row below question to divide questions
      tmp[nrow(tmp) + 1,] <- NA

    }
    # Once a full loop has completed for j, bind rows to result.
    result <- rbind(result, tmp)
  }
  # ==============================================================#
  # RETURN OR SAVE COMPILED DATA FRAME
  if (format == "statistics")
    col.names <- TRUE
  else
    col.names <- FALSE


  if (save == FALSE)
    return(result)
  else
    utils::write.table(result,
                       file = paste0(name, ".csv"),
                       na = "",
                       row.names = FALSE,
                       col.names = col.names,
                       fileEncoding = "UTF-8",
                       sep = ',')
}
