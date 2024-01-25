#' @title Visualise Big Five Personality Traits with Radar Plots
#' @name plot_bigfive
#'
#' @description
#' `plot_bigfive` creates a radar plot visualising the average scores of the Big Five
#' personality traits, supporting both individual and group comparisons. It optionally
#' accommodates weighted data.
#'
#' @param data A data frame containing survey data with Big Five personality trait scores.
#' @param bigfive A vector of column names representing the Big Five personality traits.
#'   Each trait's scores should be numeric and range from 0 to 100.
#' @param group An optional variable for comparing trait scores between different groups.
#' @param weight An optional variable containing weight factors for the analysis.
#' @param totalColour The colour used for plotting total average scores (default is a grey color).
#' @param groupColours A vector of coluors used for plotting grouped average scores
#'   (default uses a predefined palette).
#'
#' @return A ggplot2 radar plot visualising the average scores of the Big Five
#'   personality traits. The plot includes comparisons for total and, if specified,
#'   grouped averages.
#'
#' @details
#' The function performs checks to ensure that the specified `bigfive` variables exist in
#' `data` and are numeric within the range of 0 to 100. It then calculates average scores
#' (weighted or unweighted) and creates a radar plot for visual comparison. The plot
#' includes both individual trait scores and, if a `group` variable is provided, scores
#' by group.
#'
#' @examples
#' \dontrun{
#'   # Example: Create a radar plot for Big Five traits, grouped by age categories
#'   plot_bigfive(dataset,
#'                bigfive = c("Neuroticism", "Extroversion", "Openness",
#'                            "Agreeableness", "Conscientiousness"),
#'                group = "age_categories",
#'                weight = "wgtvar")
#' }
#'
#' @export
plot_bigfive <- function(data,
                         bigfive,
                         group = NULL,
                         weight = NULL,
                         totalColour = colour_pal("Regent Grey"),
                         groupColours = colour_pal("catExtended")
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

  # Function to check if all values in a vector fall within a specified range
  check_range <- function(x, min_val, max_val) {
    all(x >= min_val & x <= max_val)
  }

  # Check if all bigfive variables are within the range 0 to 100
  for (trait in bigfive) {
    if (!check_range(data[[trait]], 0, 100)) {
      stop(paste("`", trait, "` values must be between 0 and 100.", sep = ""))
    }
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
  # PLOT DATA
  # Create base grid
  p <- create_grid(bigfive)

  # Add data
  p <- add_bigfive_points_and_polygons(p, total, grouped, group, totalColour)

  p <- p +
    # Set colour for fill and colour aesthetics
    scale_fill_manual(values = groupColours,
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
#'   The metrics are labeled as 'Total' for group identification.
#'
#' @details
#' The function iterates over each Big Five trait, calculating the mean (either unweighted or
#' weighted) for each trait. It combines these results into a single data frame, with the
#' trait names as a factor.
#'
#' @noRd
get_bigfive_average <- function(data,
                                bigfive,
                                weight = NULL
) {
  result <- lapply(bigfive, function(trait) {
    mean_val <- if (is.null(weight)) {
      # Unweighted average
      mean(data[[trait]], na.rm = TRUE)
    } else {
      # Weighted average
      stats::weighted.mean(data[[trait]], data[[weight]], na.rm = TRUE)
    }
    # Add group id
    data.frame(Metric = trait, Mean = mean_val, Group = "Total")
  })

  # Combine results and make Metric a factor
  result <- do.call(rbind, result)

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
#'   The function renames the grouping column to 'Group2' and sets the 'Metric' column as a factor.
#'
#' @details
#' The function calculates the mean for each Big Five trait, grouped by the specified variable.
#' It can handle both weighted and unweighted data. The results for all traits are combined into
#' a single data frame, ensuring consistency in trait naming and grouping.
#'
#' @noRd
get_bigfive_grouped <- function(data,
                                bigfive,
                                group,
                                weight = NULL
) {
  # Evaluate and bind each metric
  result <- lapply(bigfive, function(trait) {
    tmp <- grp_mean(data, trait, group, weight)
    tmp$Metric <- trait
    return(tmp)
  })

  # Combine results and rename columns
  result <- do.call(rbind, result)

  # Rename group to "Group2"
  names(result)[names(result) == group] <- "Group2"

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

#' Add Points and Polygons for Big Five Trait Visualisation
#'
#' This internal helper function for `plot_bigfive` adds points and polygons to a ggplot
#' object to represent Big Five personality trait scores, both for total and grouped data.
#'
#' @param p The ggplot object to which the points and polygons will be added.
#' @param total A data frame containing total mean scores for Big Five traits.
#' @param grouped A data frame containing grouped mean scores for Big Five traits.
#' @param group The name of the column in `grouped` used for grouping.
#' @param totalColour The colour to be used for the total elements in the plot.
#'
#' @return A ggplot object with added points and polygons representing Big Five trait scores.
#'
#' @details
#' The function first adds points and a polygon connecting these points for the total mean scores.
#' If a group is specified, it also adds points and polygons for each group in the `grouped` data,
#' along with facet wraps to separate different groups visually.
#'
#' @noRd
add_bigfive_points_and_polygons <- function(p, total, grouped, group, totalColour) {
  p <- p +
    # Add total points for each metric
    geom_point(data = total[total$Group == "Total",],
               aes(x = Metric, y = Mean, group = Group),
               colour = totalColour) +

    # Add total polygon, joining points
    geom_polygon(data = total[total$Group == "Total",],
                 aes(x = Metric, y = Mean, group = Group),
                 alpha = 0.3, fill = totalColour, colour = totalColour)

  if (!is.null(group)) {
    p <- p +
      # Add grouped points for each metric
      geom_point(data = grouped[grouped$Group2 != "Total",],
                 aes(x = Metric, y = Mean, colour = Group2,
                     fill = Group2, group = Group2)) +
      # Add grouped polygons, joining points
      geom_polygon(data = grouped[grouped$Group2 != "Total",],
                   aes(x = Metric, y = Mean, colour = Group2,
                       fill = Group2, group = Group2), alpha = 0.3) +
      # Facet wrap groups
      facet_wrap(~Group2)
  }

  return(p)
}
