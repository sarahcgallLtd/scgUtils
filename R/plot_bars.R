#' Plot Bar Charts with Grouped Frequencies and Percentages
#' @name plot_bars
#'
#' @description
#' `plot_bars` creates bar charts to visualise the frequency and percentage of responses for a given variable,
#' optionally grouped by another variable. It supports weighting, custom rounding of percentage labels, and flexible styling options.
#'
#' @param data A data frame containing survey data.
#' @param yVar The variable to be visualized on the y-axis.
#' @param group (Optional) A variable to group the data by, creating separate plots or facets for each group.
#' @param weight (Optional) A variable containing weights to apply to each response.
#' @param round_decimals The number of decimal places to round the percentage labels to. Default is 1.
#' @param colours A named vector of colours to apply to the bars, corresponding to `yVar` categories.
#' @param title The title for the plot.
#' @param subtitle The subtitle for the plot.
#' @param width The width of the bars in the plot. Default is 0.6.
#' @param limit (Optional) The upper limit for the y-axis. If not specified, it is automatically calculated as the maximum percentage plus 10.
#' @param base_size Base font size for text elements in the plot. Default is 10.
#' @param base_font Base font family for text elements in the plot.
#'
#' @return A `ggplot2` object representing the bar chart.
#'
#' @details
#' This function leverages `ggplot2` to create aesthetically pleasing bar charts for visualising survey data. It includes features such as automatic percentage calculation, optional grouping by another variable, weighted responses, and customizable aesthetics including colours, fonts, and title/subtitle text. The function is particularly useful for summarizing categorical data and highlighting differences across groups or categories.
#'
#' @examples
#' \dontrun{
#'   data <- survey_data
#'   plot_bars(data,
#'             yVar = "question1",
#'             group = "demographic_group",
#'             weight = "survey_weight",
#'             round_decimals = 2,
#'             colours = colour_pal("catExtended"),
#'             title = "Survey Question 1 Responses",
#'             subtitle = "Grouped by Demographic",
#'             width = 0.5)
#' }
#'
#' @export
plot_bars <- function(data,
                      yVar,
                      group = NULL,
                      weight = NULL,
                      round_decimals = 1,
                      colours = colour_pal("catExtended"),
                      title = NULL,
                      subtitle = NULL,
                      width = 0.6,
                      limit = NULL,
                      base_size = 10,
                      base_font = ""
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               yVar = yVar,
               group = group,
               weight = weight)

  # ==============================================================#
  # PREPARE DATA
  plot_df <- grp_freq(data,
                      groups = c(yVar, group),
                      weight = weight,
                      addPercent = TRUE,
                      groupsPercent = group)

  if (is.null(limit)) {
    limit <- max(plot_df$Perc) + 10
  }

  # ==============================================================#
  # PLOT
  p <- ggplot(plot_df,
              aes(x = stats::reorder(!!rlang::ensym(yVar), Perc),
                  y = Perc,
                  fill = !!rlang::ensym(yVar),
                  label = sprintf(paste0("%0.", round_decimals, "f%%"), Perc)
              )) +

    # Add bars and control width
    geom_bar(stat = "identity",
             width = width) +

    # Add Labels
    geom_text(hjust = 0,
              nudge_y = 0.5,
              size = convert_sizing(base_size),
              colour = colour_pal("Black80")) +

    # Titles
    labs(title = title,
         subtitle = subtitle,
         y = "",
         x = "") +

    # Colours
    scale_fill_manual(values = colours) +

    # Control x axis
    scale_y_continuous(limits = c(0, limit),
                       expand = c(0.01, 0.01)) +

    # Flip coordinates
    coord_flip(clip="off") +

    # Add theme
    theme_scg(base_size = base_size, base_font = base_font) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank()
    )

  if (!is.null(group)) {
    p <- p +
      facet_wrap(~.data[[group]])
  }

  return(p)
}
