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
#' @param meanVar Actual age vraiable (numeric) to view average age. This variable is optional.
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
                      group,
                      weight,
                      meanVar,
                      colours,
                      title = "Population Structure",
                      subtitle,
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

  # ==============================================================#
  # GET GROUPED FREQUENCY & PERCENTAGE BY TOTAL
  # Return funciton arguments
  agg <- match.call(expand.dots = FALSE)

  # Match relevant arguments
  agg_n <- match(c("data", "weight"), names(agg), 0L)
  agg <- agg[c(1L, agg_n)]

  # Substitute `plot_popn` for `grp_freq`
  agg[[1L]] <- quote(grp_freq)

  #  Substitute `groups`
  agg[["groups"]] <- c(yVar, xVar)

  # Percent by total
  agg[["addPercent"]] <- "yes"

  # Limit decimal places
  agg[["round_decimals"]] <- 2

  # Evaluate
  total <- eval(agg, parent.frame())

  # Add id column
  total$id <- "Total"

  # ==============================================================#
  # GET AGE MEAN
  if (!missing(meanVar) && missing(group)) {
    # Return funciton arguments
    sta <- match.call(expand.dots = FALSE)

    # Match relevant arguments
    sta_n <- match(c("data", "weight"), names(sta), 0L)
    sta <- sta[c(1L, sta_n)]

    # Substitute `plot_popn` for `grp_mean`
    sta[[1L]] <- quote(grp_mean)

    # Substitute `var`
    sta[["var"]] <- meanVar

    # Substitute `group`
    sta[["group"]] <- xVar

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
  # GET GROUPED FREQUENCY & PERCENTAGE BY GROUP
  if (!missing(group)) {
    # Return funciton arguments
    grp <- match.call(expand.dots = FALSE)

    # Match relevant arguments
    grp_n <- match(c("data", "weight"), names(grp), 0L)
    grp <- grp[c(1L, grp_n)]

    # Substitute `plot_popn` for `grp_freq`
    grp[[1L]] <- quote(grp_freq)

    #  Substitute `groups`
    grp[["groups"]] <- c(group, yVar, xVar)

    # Percent by group
    grp[["groupsPercent"]] <- group

    # Limit decimal places
    grp[["round_decimals"]] <- 2

    # Evaluate
    grouped <- eval(grp, parent.frame())

    # Get linewidth
    lineWidth <- (100 / yLevels) / 2

    grouped <- convert_neg(grouped, xVar, leftLevel, "Perc")

  } else {
    lineWidth <- 100 / yLevels
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
  if (!missing(group) && missing(subtitle))
    subtitle = get_question(data, group)

  # ==============================================================#
  # PLOT
  if (missing(group)) {
    # TOTAL PLOT
    p <- ggplot(data = total,
                aes(x = Perc, y = !!rlang::ensym(yVar),
                    label = paste0(abs(Perc), "%"),
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
                    label = paste0(round(abs(Perc), 0), "%"),
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

  if (missing(group)) {
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