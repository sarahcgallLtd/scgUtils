#' @title Compile Multiple Data Frames in One
#' @name compile
#'
#' @description Produce a csv containing a full set of crosstabs or statistics for each 2x2 variables.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param row_vars Independent variable represented in rows (on the side). This parameter is required.
#' @param col_vars Dependent variable represented in columns (at the top). This parameter is required.
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
#'        row_vars = c("Q1", "Q2"),
#'        col_vars = c("Gender", "VI"),
#'        weight = "wgtvar",
#'        name = "crosstabs")
#' }
#' @importFrom magrittr %>%
#' @export
compile <- function(data,
                    row_vars,
                    col_vars,
                    weight,
                    format=c("csv", "statistics"),
                    save=TRUE,
                    name="table"
) {

  # =========== 1. CONSTRUCT FUNCTION ARGUMENTS ============ #
  xtab <- match.call(expand.dots = FALSE)
  xtab_n <- match(c("data","weight","format"), names(xtab), 0L)
  xtab <- xtab[c(1L, xtab_n)]
  xtab[[1L]] <- quote(crosstab)

  xtab[["statistics"]] <- FALSE
  xtab[["plot"]] <- FALSE

  # =========== 2. CONSTRUCT DATA FRAME ============ #
  # result <- list()
  result <- data.frame()
  for (i in row_vars) {
    xtab[["row_var"]] <- i

    temp <- data.frame()
    for (j in col_vars) {
      xtab[["col_var"]] <- j

      df <- eval(xtab, parent.frame())

      # Format for .csv
      if (format=="csv") {
        df <- rbind(NA, colnames(df), df)
        df[1,3] <- j

        if (nrow(temp) == 0)
          temp <- df
        else
          temp <- dplyr::full_join(temp, df, by=c(i,"Total"))

      } else
        temp <- rbind(temp, df)
    }
    # Format for .csv
    if (format=="csv"){
      colnames(temp) <- NULL
      temp [ nrow(temp) + 1 , ] <- NA
    }
    result <- rbind(result, temp)
  }
  # =========== 3. RETURN OR SAVE COMPILED DATA FRAME ============ #
  if (format=="statistics")
    col.names <- TRUE
  else
    col.names <- FALSE


  if (save==FALSE)
    return(result)
  else
    utils::write.table(result,
              paste0(name, ".csv"),
              na = "",
              row.names=FALSE,
              col.names=col.names,
              fileEncoding = "UTF-8",
              sep=',')
}
