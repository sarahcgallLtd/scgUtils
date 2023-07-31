#' @title Crosstab / Contingency Tables
#' @name crosstab
#'
#' @description Produce crosstabs/contingency tables to view individually and perform further analysis on.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param row_var Independent variable represented in rows (on the side). This parameter is required.
#' @param col_var Dependent variable represented in columns (at the top). This parameter is required.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param totals Logical, if totals is \code{TRUE}, df with totals column is included (default = \code{TRUE}).
#' @param round_decimals Set the number of decimal points for the data (default = \code{NULL}).
#' @param statistics Logical, if statistics is \code{TRUE}, Chi-Square, DF, Cramer's V, and p-value are printed (default = \code{TRUE}).
#' @param plot Logical, if plot is \code{TRUE}, a chart is available to be viewed (default = \code{TRUE}).
#' @param format Formatting options to return either a long or wide data frame (default = \code{"df_long"}).
#' @param convert_to Conversion options to return either a percentages or frequencies (default = \code{"percent"}).
#'
#' @return Crosstabs held in a data frame containing row-wise percentages (%) and col-wise totals (n)
#'
#' @examples
#' \dontrun{
#' # Create crosstabs with long df output using weighted data.
#' df <- crosstab(dataset,
#'               row_var = "Q1",
#'               col_var = "Gender",
#'               weight = "wgtvar",
#'               totals = FALSE,
#'               round_decimal=2)
#'}
#' @importFrom magrittr %>%
#' @export
crosstab <- function(data,
                     row_var,
                     col_var,
                     weight,
                     totals=TRUE,
                     round_decimals=NULL,
                     statistics=TRUE,
                     plot=TRUE,
                     format=c("df_long","df_wide","csv","statistics"),
                     convert_to=c("percent","frequency")
) {
  # =========== 1. CHECK DATA & ARGUMENTS ============ #
  # Check if data frame
  if (missing(data))
    stop("A data frame is required to be parsed through this function.")

  # Check if required variables are missing
  if (missing(row_var))
    stop("A row variable is required.")

  if (missing(col_var))
    stop("A column variable is required.")

  # statistics <- match.arg(statistics)
  format <- match.arg(format)
  convert_to <- match.arg(convert_to)

  # =========== 2. GET VARIABLES ============ #
  # Required variables [formula = ~ row_var + col_var]
  y <- test_deparse(row_var)
  x <- test_deparse(col_var)
  vars <- c(y, x)

  # check if weight is included [formula = weight ~ row_var + col_var]
  if (!missing(weight)){
    w <- test_deparse(weight)
    name <- "Weighted sample size"
  } else {
    w <- NULL
    name <- "Unweighted sample size"
  }

  # =========== 3. CALCULATE ============ #
  if (format=="statistics"){
    df <- xtab_calc(data, vars, w, type="statistics_df")

  } else {
    # Crosstab
    df <- xtab_calc(data, vars, w, statistics)
    df$Freq <- as.numeric(df$Freq) # ensure numeric (not integer)

    # Statistics
    stat <- xtab_calc(data, vars, w, type="statistics")

    # =========== 4. FORMATTING OPTIONS ============ #
    if (totals==TRUE)
      df <- xtab_totals(df, y, x) # add totals

    # SHAPE: Change for csv and df_wide options otherwise = df_wide or plot
    if (format %in% c("csv","df_wide")){
      # Convert to table
      df <- tidyr::pivot_wider(
        df,
        names_from = x,
        values_from = Freq
      )

    if (totals==TRUE)
      df <- df[,c(1, ncol(df), 2:(ncol(df)-1))] # Change order
    }

    # ADDITIONS:
    if (format=="csv")
      df <- xtab_totals(df, y, x, name) # Add sample sizes

    # Convert variables
    if (convert_to != "frequency")
      if (format=="csv") {
        df[-nrow(df),] <- xtabs_convert(df[-nrow(df),], convert_to, format)
      } else
        df <- xtabs_convert(df, convert_to, format) # convert to percent

    if (is.numeric(round_decimals)==TRUE)
      df <- round_vars(df, round_decimals) # round decimals

    # Add statistics row
    if (format=="csv")
      df <- dplyr::bind_rows(df, stat)

    if (statistics==FALSE)
      stat=NULL

    # Add plot
    if (plot==TRUE) {
      if (format!="df_long" || convert_to == "frequency") {
        p_df <- xtab_calc(data, vars, w, FALSE)
        if (totals==TRUE)
          p_df <- xtab_totals(p_df, y, x)
        p_df <- xtabs_convert(p_df, "percent", "df_long")
        if (is.numeric(round_decimals)==TRUE)
          p_df <- round_vars(p_df, round_decimals)
      } else {
        p_df <- df
      }

      p <- ggplot(data=p_df[p_df[2] != "Total", ],
                  aes(x= !! rlang::ensym(row_var), y=Perc,
                      fill=stats::reorder(!! rlang::ensym(col_var), Freq)))
      if (totals==TRUE) {
        p <- p + geom_bar(data=p_df[p_df[2] == "Total", ],
                          aes(x=!! rlang::ensym(row_var), y=Perc, colour=!! rlang::ensym(col_var)),
                          stat="identity",  alpha=0.4, fill="#cdcdd1") +
          scale_colour_manual(name = NULL,
                              values = "transparent",
                              guide = guide_legend(order = 2, override.aes = list(fill = "#cdcdd1")))
      }
      p <- p + geom_bar(stat="identity", width=0.8, position = position_dodge(width=0.9), alpha=1) +
        guides(fill = guide_legend(order = 1)) +
        scale_fill_manual(values=colour_pal("catExtended")) +
        scale_y_continuous(limits=c(0,100), expand=c(0,0), breaks=c(0,25,50,75,100),
                           labels=c("0%","25%","50%","75%","100%")) +
        labs(title = paste0("% of ", row_var, " by ", col_var), caption = stat,
             fill = col_var, x=row_var, y="") +
        theme_scg() +
        theme(
          plot.margin = margin(0.5,0.5,0.5,0.1, "cm"),
          panel.grid.major.x = element_blank(),
          legend.key.size = unit(0.35, 'cm')
        )
    print(p)
    }
  }
  return(df)
}
