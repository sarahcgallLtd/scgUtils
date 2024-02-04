#' @title Visualise Population Profile of Survey Data
#' @name plot_popn
#'
#' @description
#' `plot_popn` creates a visual representation of the population profile using survey data.
#' It can illustrate the distribution of age groups across genders or other categories,
#' and optionally includes average age or other statistics.
#'
#' @param data A data frame containing survey data.
#' @param xVar The gender variable (or a similar categorical variable) for the x-axis.
#' @param yVar The age group variable for the y-axis.
#' @param group An optional variable for additional grouping or comparison.
#' @param weight An optional weighting variable for the analysis.
#' @param meanVar An optional numeric variable to include average values in the plot.
#' @param colours A vector of three colors for male, female, and total. This parameter is optional.
#' @param title The title of the plot; defaults to "Population Structure".
#' @param subtitle The subtitle for the plot, particularly useful in grouped plots;
#'   defaults to the question associated with the group variable.
#' @param xLab The title for the x-axis; defaults to "Population (%)".
#' @param yLab The title for the y-axis; defaults to "Age".
#' @param addLabels A logical indicating whether to add percentage labels to the plot; defaults to `FALSE`.
#' @param thresholdLab A numeric threshold for label placement inside or outside the bars.
#' @param nudgeLab A numeric value to adjust the horizontal position of labels.
#' @param sizeLab The font size for labels.
#' @param faceLab The font style for labels; can be "plain", "bold", "italic", or "bold.italic".
#'
#' @return A `ggplot2` plot representing the population profile based on the provided survey data.
#'
#' @examples
#' \dontrun{
#'   # Example: Create a population plot with age and gender
#'   plot_popn(dataset,
#'             xVar = "gender",
#'             yVar = "age_categories",
#'             weight = "wgtvar",
#'             meanVar = "age")
#' }
#'
#' @importFrom magrittr %>%
#' @export
plot_popn <- function(data,
                      xVar,
                      yVar,
                      group = NULL,
                      weight = NULL,
                      meanVar = NULL,
                      colours = NULL,
                      title = "Population Structure",
                      subtitle = NULL,
                      xLab = "Population (%)",
                      yLab = "Age",
                      addLabels = FALSE,
                      thresholdLab = 3,
                      nudgeLab = 0.2,
                      sizeLab = 3,
                      faceLab = c("plain", "bold", "italic", "bold.italic")
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               xVar = xVar,
               yVar = yVar,
               group = group,
               weight = weight,
               meanVar = meanVar)

  # Take first option
  faceLab <- match.arg(faceLab)

  # Ensure data are factors:
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

  # ==============================================================#
  # PREPARE DATA
  prepared_data <- calculate_data(data, xVar, yVar, group, weight, meanVar)

  # Get plot attributes
  attributes <- prepare_attributes(data, xVar, yVar, group, prepared_data$stats,
                                   title, subtitle, colours)

  # Create Plot
  plot_args <- list(prepared_data, xVar, yVar, group, attributes,
                    xLab, yLab, addLabels, thresholdLab, nudgeLab,
                    sizeLab, faceLab)
  names(plot_args) <- c("prepared_data", "xVar", "yVar", "group", "attributes",
                        "xLab", "yLab", "addLabels", "thresholdLab", "nudgeLab",
                        "sizeLab", "faceLab")
  plot <- do.call("create_popn_plot", plot_args)

  # ==============================================================#
  return(plot)
}

#' Calculate Data for Population Plot
#'
#' This internal helper function for `plot_popn` prepares data for population plot visualization.
#' It calculates total and grouped frequencies, percentages, and optionally statistics.
#'
#' @param data A data frame containing the survey data.
#' @param xVar The variable for the x-axis.
#' @param yVar The variable for the y-axis.
#' @param group An optional variable for grouping.
#' @param weight An optional weighting variable for the analysis.
#' @param meanVar An optional variable to calculate mean statistics.
#'
#' @return A list containing data frames: total data, grouped data, and optionally statistics.
#'
#' @noRd
calculate_data <- function(data,
                           xVar,
                           yVar,
                           group,
                           weight,
                           meanVar
) {
  # Get Total Frequencies & Percent
  total <- grp_freq(data, groups = c(yVar, xVar), weight,
                    addPercent = TRUE, round_decimals = 2)
  # Add id column
  total$id <- "Total"

  # If group is present
  grouped <- NULL
  if (!is.null(group)) {
    # Get Grouped Frequencies & Percent
    grouped <- grp_freq(data, groups = c(group, yVar, xVar), weight,
                        groupsPercent = group, round_decimals = 2)
  }

  # If meanVar is present
  stats <- NULL
  if (!is.null(meanVar) && is.null(group)) {
    # Mean
    stats <- grp_mean(data, meanVar, groups = xVar, weight)
  }
  # ==============================================================#
  return(list(total = total, grouped = grouped, stats = stats))
}

#' Prepare Attributes for Population Plot
#'
#' An internal helper function for `plot_popn` that prepares various attributes for the plot,
#' including levels, labels, and colors.
#'
#' @param data A data frame containing the survey data.
#' @param xVar The x-axis variable in the data.
#' @param yVar The y-axis variable in the data.
#' @param group An optional grouping variable.
#' @param stats An optional data frame containing statistics.
#' @param title The title for the plot.
#' @param subtitle The subtitle for the plot.
#' @param colours A list of custom colors for the plot.
#'
#' @return A list of attributes including levels, labels, title, subtitle, line width, and colors.
#'
#' @noRd
prepare_attributes <- function(data,
                               xVar,
                               yVar,
                               group,
                               stats,
                               title,
                               subtitle,
                               colours
) {
  # Get levels
  xLevels <- levels(data[[xVar]])
  yLevels <- length(unique(data[[yVar]]))

  # Get levels
  leftLevel <- xLevels[[1]]
  rightLevel <- xLevels[[2]]

  # Get linewidth
  lineWidth <- if (!is.null(group)) (100 / yLevels) / 2 else lineWidth <- 100 / yLevels

  # Get Labels and Title for plot
  if (!is.null(stats) && is.null(group)) {
    leftLabel <- get_label(stats, leftLevel, xVar)
    rightLabel <- get_label(stats, rightLevel, xVar)
    title <- paste0(title, "\n")
  } else {
    leftLabel <- leftLevel
    rightLabel <- rightLevel
  }

  # Subtitle
  subtitle <- if (is.null(subtitle) && !is.null(group)) get_question(data, group) else subtitle

  # Get Colours
  colour <- get_colours(colours, leftLevel, rightLevel)

  return(list(xLevels = xLevels, yLevels = yLevels,
              leftLevel = leftLevel, rightLevel = rightLevel,
              leftLabel = leftLabel, rightLabel = rightLabel,
              title = title, subtitle = subtitle,
              lineWidth = lineWidth, colour = colour))
}

#' Get Label for Plot Annotation
#'
#' Internal helper function for `plot_popn` to create labels for plot annotations based on statistics.
#'
#' @param stats A data frame containing statistical information.
#' @param level The specific level within the variable to create a label for.
#' @param xVar The variable name in the `stats` data frame.
#'
#' @return A character string representing the label for annotation.
#'
#' @noRd
get_label <- function(stats,
                      level,
                      xVar
) {
  meanVal <- round(stats[stats[, xVar] == level, "Mean"], 1)
  paste0(level, "\n", "Average Age = ", meanVal, "\n")
}

#' Determine Colours for Population Plot
#'
#' Internal helper function for `plot_popn` that determines the colours to be used in the plot.
#'
#' @param customColours A list of custom colours to use in the plot.
#' @param leftLevel The level associated with the left side of the plot.
#' @param rightLevel The level associated with the right side of the plot.
#'
#' @return A named list of colours for the plot.
#'
#' @noRd
get_colours <- function(customColours,
                        leftLevel,
                        rightLevel
) {
  if (is.null(customColours)) {
    colour <- list(colour_pal("Steel Blue"),
                   colour_pal("Lilac"),
                   colour_pal("French Grey"))
  } else {
    colour <- list(customColours[[1]],
                   customColours[[2]],
                   customColours[[3]]
    )
  }
  names(colour) <- c(leftLevel, rightLevel, "Total")

  return(colour)
}

#' Create Population Plot
#'
#' This internal function for `plot_popn` creates a population plot using ggplot2,
#' based on prepared data and attributes.
#'
#' @param prepared_data A list containing prepared data frames for plotting.
#' @param xVar The variable for the x-axis.
#' @param yVar The variable for the y-axis.
#' @param group An optional grouping variable.
#' @param attributes A list of prepared attributes for the plot.
#' @param xLab Label for the x-axis.
#' @param yLab Label for the y-axis.
#' @param addLabels Logical indicating whether to add labels to the plot.
#' @param thresholdLab Threshold for label display.
#' @param nudgeLab Distance to nudge labels horizontally.
#' @param sizeLab Font size of the labels.
#' @param faceLab Font face of the labels.
#'
#' @return A ggplot object representing the population plot.
#'
#' @details
#' The function uses the provided data and attributes to create a ggplot object for
#' visualizing population data. It supports adding labels with customization options for
#' positioning, size, and style.
#'
#' @noRd
create_popn_plot <- function(prepared_data,
                             xVar,
                             yVar,
                             group,
                             attributes,
                             xLab,
                             yLab,
                             addLabels,
                             thresholdLab,
                             nudgeLab,
                             sizeLab,
                             faceLab
) {
  # ==============================================================#
  # Attributes
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Black80")
  colour <- attributes$colour

  # Prepare data: Make left side data negative
  total <- convert_neg(prepared_data$total, xVar, attributes$leftLevel, "Perc")
  if (!is.null(group)) {
    grouped <- convert_neg(prepared_data$grouped, xVar, attributes$leftLevel, "Perc")
  }

  # ==============================================================#
  # Base plot
  p <- ggplot(data = if (is.null(group)) total else grouped,
              aes(x = Perc,
                  y = !!rlang::ensym(yVar),
                  fill = !!rlang::ensym(xVar))) +

    # Add total layer
    geom_bar(data = if (is.null(group)) NULL else total, aes(group = id),
             alpha = 0.8, fill = line.col, stat = "identity") +

    # Add group/total layer
    geom_bar(stat = "identity") +

    # Add scg theme
    theme_scg() +

    coord_cartesian(clip = 'off') +

    # Add labels
    labs(title = attributes$title, fill = "",
         subtitle = if (is.null(group)) NULL else attributes$subtitle) +
    xlab(xLab) +
    ylab(yLab) +

    # Add colours
    scale_fill_manual(values = unname(unlist(colour)),
                      breaks = names(colour)) +

    # Make x axis left of butterfly plot negative
    scale_x_continuous(labels = abs) +

    # Modify theme
    theme_modifications(line.col)

  # Additional modifications based on grouping
  if (!is.null(group)) {
    p <- p +
      facet_wrap(vars(!!rlang::ensym(group))) +
      theme(legend.position = "bottom")
  } else {
    p <- add_annotations(p, attributes, yLevels = attributes$yLevels + 0.75, colour)
  }

  # Add labels if needed
  if (addLabels == TRUE) {
    data_for_labels <- if (is.null(group)) total else grouped
    p <- add_text(p, data_for_labels, "Perc", thresholdLab,
                  nudgeLab, sizeLab, faceLab, colour = text.col)
  }

  return(p)
}

#' Amend Text Labels in Geom_text Layers of a Plot
#'
#' This is an internal helper function used by `plot_population` to add and
#' customise text labels in `geom_text` layers of a `ggplot2` plot.
#'
#' @param plot The ggplot object to which text labels are to be added.
#' @param data A data frame containing the data used in the plot.
#' @param column The name of the column in `data` used for determining label characteristics.
#' @param thresholdLab A numeric threshold for determining the color of the text labels.
#' @param nudgeLab A numeric value specifying the amount of horizontal nudge for text labels.
#' @param sizeLab Font size for the text labels.
#' @param faceLab Font face for the text labels.
#' @param colour Default colour for text labels below the threshold.
#'
#' @return Returns a ggplot object with amended text labels.
#'
#' @details
#' The function adds text labels to a plot, positioning and styling them based on the specified parameters.
#' It applies conditional formatting based on the `column` values. Labels are colored white if their
#' absolute value is greater than or equal to `thresholdLab`. The horizontal position (`nudge_x`)
#' and justification (`hjust`) of labels are also adjusted based on the column values. If the `column`
#' does not exist or has no valid data, the plot is returned unmodified.
#'
#' @note Known issue: When `add_text()` is used in a facet, the horizontal justification (`hjust`)
#' does not work correctly for smaller negative values.
#'
#' @noRd
add_text <- function(plot, data, column, thresholdLab, nudgeLab, sizeLab, faceLab, colour) {
  # Check if the column exists and has values
  if (column %in% names(data) && !all(is.na(data[[column]]))) {
    label_values <- sprintf("%.2f%%", abs(data[[column]]))

    p <- plot +
      geom_text(
        aes(label = label_values),  # Ensure the column is referenced correctly
        data = data,
        size = sizeLab, fontface = faceLab, na.rm = TRUE,
        colour = ifelse(abs(data[, column]) >= thresholdLab, "white", colour),
        nudge_x = dplyr::case_when(
          abs(data[, column]) >= thresholdLab & data[, column] < 0.00 ~ nudgeLab,
          abs(data[, column]) < thresholdLab & data[, column] >= 0.00 ~ nudgeLab,
          .default = -nudgeLab
        ),
        hjust = ifelse(abs(data[, column]) >= thresholdLab, "inward", "outward"),
        check_overlap = TRUE
      )
    return(p)
  } else {
    return(plot)  # Return the plot as is if the column doesn't exist or has no valid data
  }
}

#' Theme Modifications for Population Plot
#'
#' An internal helper function for `plot_popn` to apply custom theme modifications to a ggplot.
#'
#' @param line.col The color to use for line elements in the plot theme.
#'
#' @return A ggplot theme object with custom modifications.
#'
#' @noRd
theme_modifications <- function(line.col) {
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.25, "cm"),
                                                       ends = "both"), colour = line.col))
}

#' Add Annotations to Population Plot
#'
#' An internal helper function for `plot_popn` that adds custom annotations to a ggplot object.
#'
#' @param plot The ggplot object to which annotations will be added.
#' @param attributes A list of attributes including labels for the annotations.
#' @param yLevels Numeric value specifying the y-axis level for placing annotations.
#' @param colour A named list of colors to use for each annotation.
#'
#' @return A ggplot object with added annotations.
#'
#' @details
#' This function is used to add custom text annotations to a population plot, typically used
#' for labeling specific parts of the plot with meaningful text, such as group names or summary statistics.
#'
#' @noRd
add_annotations <- function(plot, attributes, yLevels, colour) {
  plot +
    annotate("text",
             x = 1,
             y = yLevels,
             label = attributes$rightLabel,
             hjust = 0,
             fontface = "bold",
             colour = colour[[attributes$rightLevel]]) +
    annotate("text",
             x = -1,
             y = yLevels,
             label = attributes$leftLabel,
             hjust = 1,
             fontface = "bold",
             colour = colour[[attributes$leftLevel]]) +
    labs(title = attributes$title)
}