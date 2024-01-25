#' @include utils.R
NULL
#' @title Produce Crosstabs / Contingency Tables for Analysis
#' @name crosstab
#'
#' @description
#' The `crosstab` function generates crosstabs (contingency tables) from survey data,
#' allowing for a variety of formatting and analytical options. It can produce tables with
#' statistical measures, create visual plots, and handle weighted data.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param rowVar The independent variable represented in rows (side of the table).
#' @param colVar The dependent variable represented in columns (top of the table).
#' @param weight An optional variable containing weight factors for the analysis.
#' @param totals Logical; if `TRUE`, includes a totals column in the output (default is `TRUE`).
#' @param round_decimals Optional; number of decimal points for rounding the data (default is `NULL`).
#' @param statistics Logical; if `TRUE`, calculates and prints Chi-Square, degrees of freedom (DF), Cramer's V,
#'   and p-value (default is `FALSE`).
#' @param plot Logical; if `TRUE`, generates a plot for visual representation of the crosstab data (default is `FALSE`).
#' @param format Specifies the output format: 'df_long', 'df_wide', 'csv', or 'statistics' (default is 'df_long').
#' @param convert_to Determines conversion type: 'percent' or 'frequency' (default is 'percent').
#' @param yLab Title for the Y-axis, default is "Population (%)".
#' @param adjustX Logical; if `TRUE`, adjusts the X-axis labels to a 45-degree angle for better readability
#'   (default is `FALSE`).
#'
#' @return Depending on the chosen format, returns a data frame in either long or wide format,
#'   containing row-wise percentages and column-wise totals. If `plot` is `TRUE`, a plot object is also generated.
#'
#' @examples
#' \dontrun{
#'   # Example: Create crosstabs with a long data frame output using weighted data
#'   crosstab_result <- crosstab(data = dataset,
#'                               rowVar = "Q1",
#'                               colVar = "Gender",
#'                               weight = "wgtvar",
#'                               totals = FALSE,
#'                               round_decimals = 2)
#' }
#' @importFrom magrittr %>%
#' @export
crosstab <- function(data,
                     rowVar,
                     colVar,
                     weight = NULL,
                     totals = TRUE,
                     round_decimals = NULL,
                     statistics = FALSE,
                     plot = FALSE,
                     format = c("df_long", "df_wide", "csv", "statistics"),
                     convert_to = c("percent", "frequency"),
                     yLab = "Population (%)",
                     adjustX = FALSE
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               rowVar = rowVar,
               colVar = colVar,
               weight = weight)

  # Check if required variables are missing
  if (rowVar == colVar)
    stop("`rowVar` and `colVar` must be two different variables.")

  # TODO: future update to permit rowVar == colVar without error (for compile purposes)

  # take first argument if multiple
  format <- match.arg(format)
  convert_to <- match.arg(convert_to)

  # ==============================================================#
  # CALCULATE STATISTICS OR XTABS
  df <- xtab_calc(data, c(rowVar, colVar), weight, statistics,
                  type = if (format == "statistics") "statistics_df" else "xtabs_df")

  # ==============================================================#
  # FORMATTING OPTIONS
  df <- format_crosstab_data(df, rowVar, colVar, format, totals, convert_to, round_decimals, weight)

  # OPTION: STATISTICS = TRUE
  if (statistics || format == "csv") {
    stats <- xtab_calc(data, c(rowVar, colVar), weight, type = "statistics")
    # Bind stats to df if format is csv
    if (format == "csv") df <- dplyr::bind_rows(df, stats)
  } else {
    stats <- NULL
  }
  # ==============================================================#
  # OPTION: PLOT = TRUE
  if (plot == TRUE) {
    # Prepare Data
    if (format != "df_long" || convert_to == "frequency") {
      plot_df <- xtab_calc(data, c(rowVar, colVar), weight)
      plot_df <- format_crosstab_data(plot_df, rowVar, colVar, "df_long", totals,
                                      "percent", round_decimals, weight)
    } else {
      plot_df <- df
    }

    # Create crosstab plot
    plot_args <- list(plot_df, rowVar, colVar, totals, yLab, adjustX, stats)
    names(plot_args) <- c("plot_df", "rowVar", "colVar", "totals", "yLab", "adjustX", "stats")
    p <- do.call("create_crosstab_plot", plot_args)

    # Print Plot
    print(p)
  }
  # ==============================================================#
  # Return crosstab df
  return(df)
}

#' Calculate Crosstab and Retrieve Statistics
#'
#' This internal helper function for `crosstab` calculates a frequency matrix
#' and can optionally retrieve related statistics such as Chi-squared and Cramer's V.
#'
#' @param data A data frame containing the variables for crosstabulation.
#' @param formula A formula specifying the variables to be cross-tabulated.
#' @param weight Optional weighting variable.
#' @param statistics Logical, if `TRUE` returns statistical measures.
#' @param type The type of output: 'xtabs_df', 'statistics', or 'statistics_df'.
#'
#' @return Depending on the `type` parameter, returns a frequency matrix,
#'   a summary of statistics, or both in a data frame format. The statistics
#'   are calculated using the `calculate_statistics` helper function.
#'
#' @noRd
xtab_calc <- function(data,
                      formula,
                      weight = NULL,
                      statistics = FALSE,
                      type = c("xtabs_df", "statistics", "statistics_df")
) {
  # Get type argument
  type = match.arg(type)

  # Create frequency matrix
  df <- stats::xtabs(stats::reformulate(formula, response = weight), data = data)

  # Calculate statistics if required
  if (statistics || type != "xtabs_df") {
    # Calculate Statistics
    stats <- calculate_statistics(df, formula)
    if (statistics) {
      print(stats$summary)
    }
    stats_df <- data.frame(stats$summary)
    names(stats_df)[names(stats_df) == "stats.summary"] <- formula[1]
  }

  return(switch(type,
                xtabs_df = as.data.frame(df),
                statistics = stats_df,
                statistics_df = data.frame(Row_Var = formula[1],
                                           Col_Var = formula[2],
                                           Size = stats$n,
                                           Chisq = round(stats$chisq, 3),
                                           DF = stats$degrees_of_freedom,
                                           CramersV = round(stats$cramersv, 3),
                                           p_value = stats$p)
  ))
}

#' Calculate Statistics for Crosstab
#'
#' An internal helper function for `xtab_calc` that calculates statistical
#' measures such as Chi-squared, degrees of freedom, Cramer's V, and the p-value
#' for a given crosstab. It also handles cases where columns in the frequency
#' matrix contain all zeros.
#'
#' @param df A frequency matrix (data frame) from a crosstabulation.
#' @param formula The formula used for crosstabulation, used for descriptive purposes in the summary.
#'
#' @return A list containing various statistics and a summary string. The list includes
#'   the total count (`n`), Chi-squared value (`chisq`), degrees of freedom (`degrees_of_freedom`),
#'   Cramer's V (`cramersv`), p-value (`p`), and a descriptive `summary` of the statistics.
#'
#' @details
#' The function first checks and removes columns with all zeros from the frequency
#' matrix. It then calculates the specified statistical measures. The summary
#' string provides a concise description of these statistics in relation to the
#' crosstabulated variables.
#'
#' @noRd
calculate_statistics <- function(df,
                                 formula
) {
  # Check if there are columns with all zeros:
  zero_cols <- colSums(df != 0) == 0
  if (any(zero_cols)) {
    df <- df[, !zero_cols]
  }

  x = summary(df)
  k = min(length(df), nrow(df))

  # Get statistics from call
  n <- x[[2]]
  chisq <- x[[3]]
  degrees_of_freedom <- x[[4]]
  cramersv <- sqrt(chisq / (n * (k - 1)))
  p <- round(x[[6]], 3)

  # Combine stats
  summary <- paste0(formula[1], " x ", formula[2], ": ",
                    "Chisq = ", round(chisq, 3), " | ",
                    "DF = ", degrees_of_freedom, " | ",
                    "Cramer's V = ", round(cramersv, 3), " | ",
                    "p-value = ", p)

  return(list(n = n, chisq = chisq, degrees_of_freedom = degrees_of_freedom,
              cramersv = cramersv, p = p, summary = summary))
}

#' Format Crosstabulation Data
#'
#' This internal helper function for `crosstab` formats crosstabulation data
#' based on specified options such as totals, conversion to percentages, and
#' rounding decimals.
#'
#' @param df The data frame containing crosstabulation results.
#' @param rowVar The name of the row variable.
#' @param colVar The name of the column variable.
#' @param format The desired format of the output ('csv', 'df_wide', etc.).
#' @param totals Logical, if `TRUE`, includes total counts in the output.
#' @param convert_to Specifies conversion type ('percent' or 'frequency').
#' @param round_decimals The number of decimal places to round numeric values.
#' @param weight Optional weighting variable.
#'
#' @return A formatted data frame based on the specified options.
#'
#' @details
#' The function handles various formatting options including converting frequencies
#' to percentages, adding total counts, and reshaping the data frame layout. It
#' also supports rounding numerical values and adjusting the output for weighted
#' or unweighted sample sizes.
#'
#' @noRd
format_crosstab_data <- function(df,
                                 rowVar,
                                 colVar,
                                 format,
                                 totals,
                                 convert_to,
                                 round_decimals,
                                 weight
) {
  if (format != "statistics") {
    # Ensure numeric (not integer)
    df$Freq <- as.numeric(df$Freq)

    # OPTION: TOTALS = TRUE
    if (totals) df <- xtab_totals(df, rowVar, colVar)

    # OPTION: FORMAT = "csv" or "df_wide"
    if (format %in% c("csv", "df_wide")) {
      # Convert to wide crosstab table
      df <- pivot_wide(df, c(rowVar, colVar))

      # Move Totals to 2nd column if present
      if (totals) df <- df[, c(1, ncol(df), 2:(ncol(df) - 1))]

      # Add sample size to each column
      if (format == "csv") {
        name <- if (!is.null(weight)) "Weighted sample size" else "Unweighted sample size"
        df <- xtab_totals(df, rowVar, colVar, name)
      }
    }

    # OPTION: CONVERT_TO = "percent"
    if (convert_to != "frequency")
      if (format == "csv") {
        # Convert all to percent except sample size at end of each column
        df[-nrow(df),] <- xtabs_convert(df[-nrow(df),], convert_to, format)
      } else
        # convert to percent
        df <- xtabs_convert(df, convert_to, format)

    # OPTION: ROUND_DECUIMALS != NULL & IS NUMERIC
    df <- round_vars(df, round_decimals)

  }
  return(df)
}

#' Add Totals and Sample Size to Data Frame
#'
#' An internal helper function for `crosstab` that appends total counts and
#' sample sizes to the crosstabulation data frame.
#'
#' @param data The crosstabulation data frame.
#' @param rowVar The name of the row variable.
#' @param colVar The name of the column variable.
#' @param name Optional name for the totals row.
#'
#' @return A data frame with appended totals and sample size.
#'
#' @noRd
xtab_totals <- function(data,
                        rowVar,
                        colVar,
                        name = NULL
) {
  if (is.null(name)) {
    # Create column-wise totals
    total <- stats::aggregate(data$Freq, by = list(y = data[, 1]), FUN = sum)
    total$z <- "Total"
    names(total)[names(total) == "x"] <- "Freq"
    names(total)[names(total) == "y"] <- rowVar
    names(total)[names(total) == "z"] <- colVar
    df <- dplyr::bind_rows(data, total)
  } else {
    # Create row-wise totals
    df <- janitor::adorn_totals(
      data,
      where = "row",
      name = name
    )
  }
  return(df)
}

#' Convert Frequencies to Percentages in Crosstab
#'
#' This internal helper function for `crosstab` converts frequency counts
#' to percentages in the crosstabulation data frame.
#'
#' @param data The crosstabulation data frame.
#' @param convert_to The type of conversion, currently supports 'percent'.
#' @param format The format of the output, such as 'df_long' or 'csv'.
#'
#' @return A data frame with frequencies converted to percentages.
#'
#' @noRd
xtabs_convert <- function(data,
                          convert_to,
                          format
) {
  if (convert_to == "percent") {
    if (format == ("df_long")) {
      data <- transform(data, `Perc` = stats::ave(Freq, data[, 2], FUN = prop.table))
      data$Perc <- as.numeric(data$Perc * 100)
    } else {
      # Determine which function to use based on the 'format'
      conversion_func <- if (format == "csv") proportion else percent
      # Apply the selected function to all numeric columns
      data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], FUN = conversion_func)
    }
  }
  return(data)
}

#' Create Crosstab Plot
#'
#' This internal helper function for `crosstab` creates a plot for visualising
#' crosstabulation results. It supports the inclusion of total bars and
#' customisation of various plot aesthetics.
#'
#' @param plot_df Data frame containing the crosstabulation results.
#' @param rowVar The name of the row variable.
#' @param colVar The name of the column variable.
#' @param totals Logical, if `TRUE`, includes total bars in the plot.
#' @param yLab Y-axis label, defaults to "Population (%)".
#' @param adjustX Logical, if `TRUE`, adjusts the X-axis text for better readability.
#' @param stats Optional string of statistics to include as a caption.
#' @param line.col The color for the total bars, defaults to a grey color from `colour_pal`.
#'
#' @return A `ggplot` object visualising the crosstabulation data.
#'
#' @details
#' The function creates a bar plot with options to adjust aesthetics like
#' bar colors, axis labels, and the inclusion of totals. It also handles NaN
#' values and allows for the inclusion of additional statistical information
#' as a plot caption.
#'
#' @noRd
create_crosstab_plot <- function(plot_df,
                                 rowVar,
                                 colVar,
                                 totals,
                                 yLab = "Population (%)",
                                 adjustX = FALSE,
                                 stats = NULL,
                                 line.col = colour_pal("French Grey")) {
  # Omit NaNs from graph
  p_df <- stats::na.omit(plot_df)

  # Create plot
  p <- ggplot(data = p_df[p_df[2] != "Total",],
              aes(x = !!rlang::ensym(rowVar), y = Perc,
                  fill = stats::reorder(!!rlang::ensym(colVar), Freq)))

  if (totals) {
    p <- p +
      # Add total bars
      geom_bar(data = p_df[p_df[2] == "Total",],
               aes(colour = !!rlang::ensym(colVar)),
               stat = "identity", alpha = 0.4, fill = line.col) +

      # Set colour of total bar
      scale_colour_manual(name = NULL,
                          values = "transparent",
                          guide = guide_legend(order = 2, override.aes = list(fill = line.col)))
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
         caption = stats,
         fill = get_question(p_df, colVar),
         x = get_question(p_df, rowVar),
         y = yLab) +

    # Add scg theme
    theme_scg() +

    # Remove horizontal grid lines and reduce size of legend keys
    theme(panel.grid.major.x = element_blank(),
          legend.key.size = unit(0.35, 'cm'))

  # Adjust x values
  if (adjustX) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }

  return(p)
}