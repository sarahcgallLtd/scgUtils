#' @title Big Five Personality Radar Plots
#' @name plot_bigfive
#'
#' @description Visualises the average scores of the Big Five personality traits on a radar graph.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param bigfive A vector with the column names representing the Big Five personality trait scores. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
#'
#' @return A radar plot created using ggplot2, displaying the average scores of the Big Five personality traits.
#'
#' @examples
#' \dontrun{
#' # Create a radar plot using age groups
#' plot_bigfive(dataset,
#'              bigfive = c("Neuroticism", "Extroversion", "Openness",
#'                           "Agreeableness", "Conscientiousness"),
#'              group = "age_categories",
#'              weight = "wgtvar")
#'}
#' @export
plot_bigfive <- function(data,
                         bigfive,
                         group = NULL,
                         weight = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               group = group,
               weight = weight)

  # Check if bigfive variables are in the data frame
  stopifnot("`bigfive` variable must be a column in `data`." = all(bigfive %in% names(data)))

  # Check if bigfive variables are numeric (0-100)
  for (trait in bigfive) {
    stopifnot("`bigfive` must be numeric (0-100)." = is.numeric(data[[trait]]))
  }

  # ==============================================================#
  # GET AVERAGE OF BIG FIVE
  # Get the average of Big Five by total
  total <- get_bigfive_average(data, bigfive, weight)

  # Get the average of Big Five by group
  if (!is.null(group)) {
    grouped <- get_bigfive_grouped(data, bigfive, group, weight)
  }

  # ==============================================================#
  # PREPARE ATTRIBUTES & GRID
  # Colours
  text.col <- colour_pal("Regent Grey")
  p <- create_grid(bigfive)

  # ==============================================================#
  # PLOT DATA
  p <- p +
    # Add total points for each metric
    geom_point(data = total[total$Group == "Total",],
               aes(x = Metric, y = Mean, group = Group),
               colour = text.col) +

    # Add total polygon, joining points
    geom_polygon(data = total[total$Group == "Total",],
                 aes(x = Metric, y = Mean, group = Group),
                 alpha = 0.3,
                 fill = text.col,
                 colour = text.col)

  if (!is.null(group)) {
    p <- p +
      # Add grouped points for each metric
      geom_point(data = grouped[grouped$Group2 != "Total",],
                 aes(x = Metric, y = Mean, colour = Group2,
                     fill = Group2, group = Group2)) +

      # Add grouped polygons, joining points
      geom_polygon(data = grouped[grouped$Group2 != "Total",],
                   aes(x = Metric, y = Mean, colour = Group2,
                       fill = Group2, group = Group2),
                   alpha = 0.3) +

      # Facet wrap groups
      facet_wrap(~Group2)
  }
  p <- p +
    # Set colour for fill and colour aesthetics
    scale_fill_manual(values = colour_pal("catExtended"),
                      aesthetics = c("colour", "fill")) +

    # Set y axis limits
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

  return(p)
}


#' Calculate Big Five Trait Averages
#'
#' This internal helper function for `plot_bigfive` calculates the average scores
#' for Big Five personality traits, either weighted or unweighted.
#'
#' @param data A data frame containing the Big Five trait scores.
#' @param bigfive A vector of column names corresponding to the Big Five traits.
#' @param weight An optional column name in `data` to be used for weighted averages.
#'
#' @return A data frame with the mean scores for each Big Five trait and a grouping variable.
#'
#' @noRd
get_bigfive_average <- function(data,
                                bigfive,
                                weight = NULL) {
  result <- data.frame()
  for (trait in bigfive) {
    if (!is.null(weight)) {
      # Weighted average
      tmp <- data.frame(Mean = stats::weighted.mean(data[[trait]], data[[weight]]))
    } else {
      # Unweighted average
      tmp <- data.frame(Mean = sum(data[[trait]]) / nrow(data))
    }
    # Add big five name to column
    tmp$Metric <- trait

    # Add combine data
    result <- rbind(result, tmp)
  }

  # Add group id
  result$Group <- "Total"

  # Add grouped to total
  result <- rbind(result, result[result[, "Metric"] == bigfive[1],])

  # Make metrics factors
  result$Metric <- factor(result$Metric, levels = bigfive)

  return(result)

}

#' Calculate Grouped Big Five Trait Averages
#'
#' An internal helper function for `plot_bigfive` that calculates averages of
#' Big Five personality traits, grouped by a specified variable, with optional weighting.
#'
#' @param data A data frame containing the Big Five trait scores.
#' @param bigfive A vector of column names corresponding to the Big Five traits.
#' @param group The name of the column in `data` used for grouping.
#' @param weight An optional column name in `data` for weighted averages.
#'
#' @return A data frame with the mean scores for each Big Five trait, grouped by the specified variable.
#'
#' @noRd
get_bigfive_grouped <- function(data,
                                bigfive,
                                group,
                                weight = NULL
) {
  # Evaluate each metric
  result <- data.frame()
  for (trait in bigfive) {
    tmp <- grp_mean(data, trait, group, weight)
    tmp$Metric <- trait
    result <- rbind(result, tmp)
  }

  # Rename group to "Group2"
  names(result)[names(result) == group] <- "Group2"

  # Bind rows
  result <- rbind(result, result[result[, "Metric"] == bigfive[1],])

  # Make metrics factors
  result$Metric <- factor(result$Metric, levels = bigfive)

  return(result)
}

#' Create Circular Grid for Big Five Radar Plot
#'
#' This internal helper function for `plot_bigfive` creates a circular grid for
#' radar plot visualization of Big Five personality traits.
#'
#' @param outerLabs A vector of labels for the outermost points of the grid.
#' @param line.col Colour for the grid lines.
#' @param text.col Colour for the text labels.
#'
#' @return A `ggplot` object representing a circular grid for a radar plot.
#'
#' @noRd
create_grid <- function(outerLabs,
                        line.col = colour_pal("French Grey"),
                        text.col = colour_pal("Regent Grey")) {

  grid <- data.frame(
    x = rep(outerLabs, each = length(seq(0, 100, by = 10))),
    y = rep(seq(0, 100, by = 10), length(outerLabs))
  )

  # Duplicate the first outerLabs to ensure the grid is fully enclosed
  grid <- rbind(grid, grid[grid[, "x"] == outerLabs[1],])

  # Make xVar/outerLabs factor
  grid$x <- factor(grid$x, levels = outerLabs)

  labels <- data.frame(
    x = c(0.5, 0.5, 0.5, 0.5),
    y = c(81, 60.75, 40.75, 20.25),
    label = c("100", "75", "50", "25")
  )

  p <- ggplot(data = grid, aes(x = x, y = y, group = y)) +
    geom_path(colour = line.col, linewidth = 0.15, alpha = 0.5) +
    geom_path(data = grid[grid$y %in% c(50, 100),], aes(x = x, y = y, group = y), linewidth = 0.25) +
    geom_path(data = unique(grid), aes(x = x, y = y, group = x), colour = line.col, linewidth = 0.25, linetype = "dashed") +
    geom_label(data = labels, aes(x = x, y = y, label = label), size = 4, fill = "white", colour = text.col,
               label.padding = unit(0.0, "lines"), label.size = 0, label.r = unit(0.0, "lines")) +
    coord_radar() +
    theme_scg() +
    theme(
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none"
    )

  return(p)
}

