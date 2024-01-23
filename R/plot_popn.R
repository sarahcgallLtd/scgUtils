#' @title Population Profile Plot
#' @name plot_popn
#'
#' @description Visualise the population profile of survey data using age categories and gender.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param xVar Gender variable. This parameter is required.
#' @param yVar Age group variable. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param meanVar Actual age variable (numeric) to view average age. This variable is optional.
#' @param colours A vector of three colours for Male, Female, and Total
#' @param title Plot title. Default = "Population Structure".
#' @param subtitle Plot subtitle in grouped/faceted plot. Default = question of group variable.
#' @param xLab X axis title. Default = "Population (%)".
#' @param yLab Y axis title. Default = "Age".
#' @param addLabels Option to add % labels to graph. Default = "no".
#' @param thresholdLab Numeric value to determine threshold of which labels go inside or outside of bars. Default = 3.
#' @param nudgeLab Numeric value to nudge labels left or right. Default = 0.2.
#' @param sizeLab Numeric value to determine size of label text. Default = 3.
#' @param faceLab Option to make labels "plain", "bold", "italic", or "bold.italic". Default = "plain".
#'
#' @return ggplot2 plot of the population structure
#'
#' @examples
#' \dontrun{
#' # Create plot using age groups and age intervals included to return average age
#' plot_popn(dataset,
#'           age_groups = "age_categories",
#'           gender = "gender",
#'           weight = "wgtvar",
#'           age_int = "age")
#'}
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
                      addLabels = c("no", "yes"),
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
  addLabels <- match.arg(addLabels)
  faceLab <- match.arg(faceLab)

  # ==============================================================#
  # PREPARE VARIABLES
  # Ensure all character classes are converted to factors
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

  # Get `x_var` levels
  xLevels <- levels(data[[xVar]])

  # Get levels
  leftLevel <- xLevels[[1]]
  rightLevel <- xLevels[[2]]

  # Get number of factors
  yLevels <- length(unique(data[[yVar]]))

  # Return funciton arguments
  args <- match.call(expand.dots = FALSE)

  # Match relevant arguments
  args_n <- match(c("data", "weight"), names(args), 0L)
  args <- args[c(1L, args_n)]

  # Substitute `plot_popn` for `grp_freq`
  args[[1L]] <- quote(grp_freq)

  # Limit decimal places
  args[["round_decimals"]] <- 2

  # ==============================================================#
  # GET GROUPED FREQUENCY & PERCENTAGE BY TOTAL
  #  Substitute `groups`
  args[["groups"]] <- c(yVar, xVar)

  # Percent by total
  args[["addPercent"]] <- "yes"

  # Evaluate
  total <- eval(args, parent.frame())

  # Add id column
  total$id <- "Total"

  # ==============================================================#
  # GET GROUPED FREQUENCY & PERCENTAGE BY GROUP
  if (!is.null(group)) {
    #  Substitute `groups`
    args[["groups"]] <- c(group, yVar, xVar)

    # Percent by group
    args[["groupsPercent"]] <- group

    # Evaluate
    grouped <- eval(args, parent.frame())

    # Get linewidth
    lineWidth <- (100 / yLevels) / 2

    # Convert left side as negative
    grouped <- convert_neg(grouped, xVar, leftLevel, "Perc")

  } else {
    lineWidth <- 100 / yLevels
  }

  # ==============================================================#
  # GET AGE MEAN
  if (!is.null(meanVar) && is.null(group)) {
    # Return funciton arguments
    sta <- match.call(expand.dots = FALSE)

    # Match relevant arguments
    sta_n <- match(c("data", "meanVar", "weight"), names(sta), 0L)
    sta <- sta[c(1L, sta_n)]

    # Substitute `plot_popn` for `grp_mean`
    sta[[1L]] <- quote(grp_mean)

    # Substitute `group`
    sta[["groups"]] <- xVar

    # Evaluate
    stats <- eval(sta, parent.frame())

    # Get Labels for plot
    leftLabel <- paste0(leftLevel, "\n",
                        "Average Age = ", round(stats[stats[, xVar] == leftLevel, "Mean"], 1),
                        "\n")
    rightLabel <- paste0(rightLevel, "\n",
                         "Average Age = ", round(stats[stats[, xVar] == rightLevel, "Mean"], 1),
                         "\n")
    title <- paste0(title, "\n")
  } else {
    leftLabel <- leftLevel
    rightLabel <- rightLevel
  }

  # ==============================================================#
  # PREPARE ATTRIBUTES
  # Colours
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Black80")

  if (missing(colours)) {
    colour <- list(colour_pal("Steel Blue"),
                   colour_pal("Lilac"),
                   colour_pal("French Grey")
    )
  } else {
    colour <- list(colours[[1]],
                   colours[[2]],
                   colours[[3]]
    )
  }

  names(colour) <- c(leftLevel, rightLevel, "Total")

  # Make left side data negative
  total <- convert_neg(total, xVar, leftLevel, "Perc")

  # Subtitle
  if (!is.null(group) && is.null(subtitle))
    subtitle = get_question(data, group)

  # ==============================================================#
  # PLOT
  if (is.null(group)) {
    # TOTAL PLOT
    p <- ggplot(data = total,
                aes(x = Perc, y = !!rlang::ensym(yVar),
                    label = sprintf("%.2f%%", abs(Perc)),
                    fill = !!rlang::ensym(xVar))) +

      # Add total layer
      geom_bar(stat = "identity") +

      # Add scg theme
      theme_scg() +

      # Remove legend
      theme(legend.position = "none")

    # Add labels
    if (addLabels == "yes") {
      p <- add_text(p, total, "Perc", thresholdLab,
                    nudgeLab, sizeLab, faceLab, colour = text.col)
    }

  } else {
    # GROUP PLOT
    p <- ggplot(data = grouped,
                aes(x = Perc, y = !!rlang::ensym(yVar),
                    label = sprintf("%.0f%%", abs(Perc)),
                    fill = !!rlang::ensym(xVar)
                )) +

      # Add total layer
      geom_bar(data = total, aes(group = id),
               alpha = 0.8, fill = line.col, stat = "identity") +

      # Add grouped layer
      geom_bar(stat = "identity") +

      # Facet by group
      facet_wrap(vars(!!rlang::ensym(group))) +

      # Add title, remove legend title, and add survey question as subtitle
      labs(title = title, fill = "",
           subtitle = subtitle) +

      # Add scg theme
      theme_scg() +

      # Include legend at bottom
      theme(legend.position = "bottom")

    # Add labels
    if (addLabels == "yes") {
      p <- add_text(p, grouped, "Perc", thresholdLab,
                    nudgeLab, sizeLab, faceLab, colour = text.col)
    }
  }

  p <- p +
    # Add axes titles
    xlab(xLab) +
    ylab(yLab) +

    # Add colours to
    scale_fill_manual(values = unname(unlist(colour)),
                      breaks = names(colour)) +

    # Make x axis left of butterfly plot negative
    scale_x_continuous(labels = abs) +

    # Remove horizontal gridlines and axes ticks and lines
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.25, "cm"),
                                                     ends = "both"),
                                 colour = line.col
      )
    )

  if (is.null(group)) {
    p <- p +
      # Allow labels to go off plot areas
      coord_cartesian(clip = 'off') +

      # Add right-side label at top
      annotate("text",
               x = 1,
               y = yLevels + 0.75,
               label = rightLabel,
               hjust = 0,
               fontface = "bold",
               colour = colour[[rightLevel]]) +

      # Add left-side label at top
      annotate("text",
               x = -1,
               y = yLevels + 0.75,
               label = leftLabel,
               hjust = 1,
               fontface = "bold",
               colour = colour[[leftLevel]]) +

      # Add title
      labs(title = title)
  }
  # ==============================================================#
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
#' The function modifies text labels based on the value in `column`. Labels are colored
#' white if the absolute value of the data in `column` is greater than or equal to `thresholdLab`,
#' otherwise the specified `colour` is used. The horizontal position (`nudge_x`) and
#' horizontal justification (`hjust`) of labels are also adjusted based on the value in `column`.
#'
#' @note Known issue: When `add_text()` is used in a facet, the horizontal justification
#' (`hjust`) does not work correctly for smaller negative values.
#'
#' @noRd
add_text <- function(plot, data, column, thresholdLab, nudgeLab, sizeLab, faceLab, colour) {
  p <- plot +
    geom_text(
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
}