#' @title Crosstab / Contingency Tables
#' @name crosstab
#'
#' @description Produce crosstabs/contingency tables to view individually and perform further analysis on.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param rowVar Independent variable represented in rows (on the side). This parameter is required.
#' @param colVar Dependent variable represented in columns (at the top). This parameter is required.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param totals Logical, if totals is \code{TRUE}, df with totals column is included (default = \code{TRUE}).
#' @param round_decimals Set the number of decimal points for the data (default = \code{NULL}).
#' @param statistics Logical, if statistics is \code{TRUE}, Chi-Square, DF, Cramer's V, and p-value are printed (default = \code{TRUE}).
#' @param plot Logical, if plot is \code{TRUE}, a chart is available to be viewed (default = \code{TRUE}).
#' @param format Formatting options to return either a long or wide data frame (default = \code{"df_long"}).
#' @param convert_to Conversion options to return either a percentages or frequencies (default = \code{"percent"}).
#' @param yLab Y axis title. Default = "Population (%)".
#'
#' @return Crosstabs held in a data frame containing row-wise percentages (%) and col-wise totals (n)
#'
#' @examples
#' \dontrun{
#' # Create crosstabs with long df output using weighted data.
#' df <- crosstab(dataset,
#'               rowVar = "Q1",
#'               colVar = "Gender",
#'               weight = "wgtvar",
#'               totals = FALSE,
#'               round_decimal=2)
#'}
#' @importFrom magrittr %>%
#' @export
crosstab <- function(data,
                     rowVar,
                     colVar,
                     weight,
                     totals = TRUE,
                     round_decimals = NULL,
                     statistics = TRUE,
                     plot = TRUE,
                     format = c("df_long", "df_wide", "csv", "statistics"),
                     convert_to = c("percent", "frequency"),
                     yLab = "Population (%)"
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               rowVar = rowVar,
               colVar = colVar,
               weight = weight)

  # Check if required variables are missing
  if (missing(rowVar) && missing(colVar))
    stop("`rowVar` and `colVar` are required to be parsed through this function.")

  # statistics <- match.arg(statistics)
  format <- match.arg(format)
  convert_to <- match.arg(convert_to)

  # ==============================================================#
  # PREPARE VARIABLES
  # Return funciton arguments
  tmp <- match.call(expand.dots = FALSE)

  # Match relevant arguments
  tmp_n <- match(c("data", "weight", "statistics"), names(tmp), 0L)
  tmp <- tmp[c(1L, tmp_n)]

  # Substitute `crosstab` for `xtab_calc`
  tmp[[1L]] <- quote(xtab_calc)

  #  Substitute `formula`
  tmp[["formula"]] <- c(rowVar, colVar)

  # check if weight is included [formula = weight ~ rowVar + colVar]
  if (!missing(weight)) {
    tmp[["weight"]] <- weight
    name <- "Weighted sample size"
  } else
    name <- "Unweighted sample size"

  # ==============================================================#
  # CALCULATE XTABS
  if (format == "statistics") { # Statistics only
    #  Substitute `type`
    tmp[["type"]] <- "statistics_df"

    # Evaluate: xtab_calc(data, c(rowVar, colVar), weight, type = "statistics_df")
    df <- eval(tmp, parent.frame())

  } else { # Crosstab
    #  Substitute `statistics`
    tmp[["statistics"]] <- statistics

    # Evaluate: xtab_calc(data, c(rowVar, colVar), weight, statistics)
    df <- eval(tmp, parent.frame())

    # Ensure numeric (not integer)
    df$Freq <- as.numeric(df$Freq)

    #  Substitute `type`
    tmp[["type"]] <- "statistics"

    #  Substitute `statistics` (turn off)
    tmp[["statistics"]] <- FALSE

    # Evaluate Statistics: xtab_calc(data, c(rowVar, colVar), weight, type = "statistics")
    stat <- eval(tmp, parent.frame())

    # ==============================================================#
    # FORMATTING OPTIONS
    if (totals == TRUE)
      # Add totals
      df <- xtab_totals(df, rowVar, colVar)

    # SHAPE: Change for csv and df_wide options otherwise = df_wide or plot
    if (format %in% c("csv", "df_wide")) {
      # Convert to table
      df <- pivot_wide(df, c(rowVar, colVar))

      if (totals == TRUE)
        # Change order
        df <- df[, c(1, ncol(df), 2:(ncol(df) - 1))]
    }

    # ADDITIONS:
    if (format == "csv")
      # Add sample sizes
      df <- xtab_totals(df, rowVar, colVar, name)

    # Convert variables
    if (convert_to != "frequency")
      if (format == "csv") {
        df[-nrow(df),] <- xtabs_convert(df[-nrow(df),], convert_to, format)
      } else
        # convert to percent
        df <- xtabs_convert(df, convert_to, format)

    if (is.numeric(round_decimals) == TRUE)
      # round decimals
      df <- round_vars(df, round_decimals)

    # Add statistics row
    if (format == "csv")
      df <- dplyr::bind_rows(df, stat)

    if (statistics == FALSE)
      stat = NULL

    # Add plot
    if (plot == TRUE) {
      if (format != "df_long" || convert_to == "frequency") {
        # Substitute type
        tmp[["type"]] <- "xtabs_df"

        # Evaluate: xtab_calc(data = df, weight = weight, statistics = FALSE,  formula = vars, type = "xtabs_df")
        p_df <- eval(tmp, parent.frame())

        if (totals == TRUE)
          p_df <- xtab_totals(p_df, rowVar, colVar)

        # Convert to percentage
        p_df <- xtabs_convert(p_df, "percent", "df_long")

        if (is.numeric(round_decimals) == TRUE)
          # Round to x decimal places
          p_df <- round_vars(p_df, round_decimals)

      } else {
        p_df <- df
      }

      # Omit NaNs from graph
      p_df <- stats::na.omit(p_df)

      # Colours
      line.col <- colour_pal("French Grey")

      # ==============================================================#
      # PLOT
      p <- ggplot(data = p_df[p_df[2] != "Total",],
                  aes(x = !!rlang::ensym(rowVar), y = Perc,
                      fill = stats::reorder(!!rlang::ensym(colVar), Freq)))

      if (totals == TRUE) {
        p <- p +
          # Add total bars
          geom_bar(data = p_df[p_df[2] == "Total",],
                   aes(x = !!rlang::ensym(rowVar), y = Perc, colour = !!rlang::ensym(colVar)),
                   stat = "identity", alpha = 0.4, fill = line.col) +

          # Set colour of total bar
          scale_colour_manual(name = NULL,
                              values = "transparent",
                              guide = guide_legend(order = 2,
                                                   override.aes = list(fill = line.col)))
      }
      p <- p +
        # Add grouped bars
        geom_bar(stat = "identity", width = 0.8,
                 position = position_dodge(width = 0.9), alpha = 1) +

        # Set order of legend
        guides(fill = guide_legend(order = 1)) +

        # Add colours to bars
        scale_fill_manual(values = colour_pal("catExtended")) +

        # Limit y axis from 0 to 100
        scale_y_continuous(limits = c(0, 100), expand = c(0, 0),
                           breaks = c(0, 25, 50, 75, 100)) +

        # Add labels
        labs(title = paste0("% of ", rowVar, " by ", colVar),
             caption = stat,
             fill = colVar,
             x = rowVar,
             y = yLab) +

        # Add scg theme
        theme_scg() +

        # Remove horizontal grid lines and reduce size of legend keys
        theme(
          panel.grid.major.x = element_blank(),
          legend.key.size = unit(0.35, 'cm')
        )
      print(p)
    }
  }
  # ==============================================================#
  return(df)
}
