#' @title Create a Waffle Plot
#' @name plot_waffle
#'
#' @description
#' `plot_waffle` creates a waffle plot to visualise categorical data. This plot is useful for
#' showing proportions or percentages in a visually appealing and easily understandable format.
#'
#' @param data A data frame containing the dataset to be visualized.
#' @param group The column in `data` that contains the categorical groups for the waffle plot.
#' @param values Optional: The column in `data` that contains the numerical values used for
#'   calculating proportions. If not provided, counts of `group` categories are used.
#' @param weight Optional: A weighting variable to be used in calculating group frequencies.
#' @param isolateVar Optional: A specific variable from `group` to isolate in the plot.
#' @param orderPlots Specifies how the plots should be ordered. Accepted values are 'none',
#'   'ascending', or 'descending'. Default is 'none'.
#' @param title The title of the waffle plot. If the title matches the group name, the function
#'    will return the label attribute if present.
#' @param groupColours A named or unnamed vector of colours for each category in the plot.
#' @param backgroundColour Colour for the background of the plot.
#' @param backgroundAlpha Alpha value (transparency) for the background.
#' @param borderColour Colour for the border of the tiles in the plot.
#' @param borderWidth Width of the border of the tiles. Default = 0.5.
#'
#' @return A `ggplot` object representing a waffle plot.
#'
#' @examples
#'   data <- data.frame(
#'     Category = c("A", "B", "C"),
#'     Count = c(30, 40, 30)
#'   )
#'   plot_waffle(data,
#'               group = "Category",
#'               values = "Count",
#'               title = "Sample Waffle Plot",
#'               orderPlots = "descending",
#'               groupColours = c(A = "blue", B = "yellow", C = "red"))
#'
#' @export
plot_waffle <- function(data,
                        group,
                        values = NULL,
                        weight = NULL,
                        isolateVar = NULL,
                        orderPlots = c("none", "descending", "ascending"),
                        title = NULL,
                        groupColours = colour_pal("catExtended"),
                        backgroundColour = colour_pal("French Grey"),
                        backgroundAlpha = 0.5,
                        borderColour = "white",
                        borderWidth = 0.5
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               group = group,
               values = values,
               weight = weight)

  if (!is.null(isolateVar) && !all(isolateVar %in% data[[group]])) {
    stop("`isolateVar` must be in `group`.")
  }

  orderPlots <- match.arg(orderPlots)

  # ==============================================================#
  # PREPARE DATA
  # Calculate proportions and apply to grid data
  calculated_data <- calculate_proportions(data, group, values, weight)

  # Create grid data by iterating through the create_grid function for each variable in group
  grid_list <- lapply(unique(calculated_data[[group]]), function(x) create_waffle_grid(calculated_data, group, x, orderPlots))
  accumulated_grid <- do.call(rbind, grid_list)

  # Filter by isolateVar if it is not NULL
  if (!is.null(isolateVar)) {
    accumulated_grid <- subset(accumulated_grid, variable == isolateVar)
  }

  # ==============================================================#
  # PREPARE ATTRIBUTES
  # If title is the same as group, check if label exists
  if (!is.null(title) && title == group) {
    title <- get_question(data, title)
  }

  # Ensure varColours has names
  if (is.null(names(groupColours))) {
    names(groupColours) <- unique(calculated_data[[group]])
  }
  groupColours["zzz"] <- backgroundColour

  # ==============================================================#
  # PLOT
  p <- create_waffle_plot(accumulated_grid, orderPlots, title, groupColours,
                          backgroundColour, backgroundAlpha,
                          borderColour, borderWidth)

  # ==============================================================#
  return(p)
}


#' Calculate Proportions for Waffle Plot
#'
#' @description
#' This internal function calculates the proportions for each group in a dataset and determines
#' the number of squares each category should get in a waffle plot.
#'
#' @param data A data frame containing the dataset to be visualized.
#' @param group The column in `data` that contains the categorical groups.
#' @param values The column in `data` that contains the values used to calculate proportions.
#' @param weight Optional: A weighting variable to be used in calculating group frequencies.
#'
#' @return A modified data frame with added percentage column indicating proportions of each group.
#'
#' @noRd
calculate_proportions <- function(data,
                                  group,
                                  values,
                                  weight
) {
  if (is.null(values)) {
    data <- grp_freq(data,
                     groups = group,
                     weight = weight,
                     addPercent = TRUE,
                     round_decimals = 0)

  } else {
    total_values <- sum(data[[values]])
    data$Perc <- round((data[[values]] / total_values) * 100)
  }

  # Make group character:
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], as.character)

  return(data)
}

#' Create Grid for Waffle Plot
#'
#' @description
#' Generates a grid layout for a waffle plot for a specific variable from a dataset.
#'
#' @param data The preprocessed data frame with calculated proportions.
#' @param group The column in `data` indicating categorical groups.
#' @param variable The specific variable for which the grid is being created.
#' @param orderPlots Specification for ordering plots, if any.
#'
#' @return A data frame representing a grid layout for the specified variable in the waffle plot.
#'
#' @noRd
create_waffle_grid <- function(data,
                               group,
                               variable,
                               orderPlots
) {
  # Calculate the number of squares to fill for the given variable
  data_subset <- subset(data, data[[group]] == variable)

  # Initialise the category column with "zzz"
  data_zzz <- subset(data, data[[group]] != variable)

  # Fill in the squares for the given variable
  num_squares <- sum(data_subset$Perc)

  # Create a base grid
  grid_data <- expand.grid(x = 1:10, y = 1:10)

  # Assign categories to each cell in base grid
  grid_data$category <- rep(c(variable, "zzz"), c(num_squares, 100 - num_squares))

  # Add the variable name with the percentage for facet labels
  grid_data$variable <- variable
  grid_data$label <- paste0(variable, " (", num_squares, "%)")

  # Add the ordering for the facets based on the specified variable order
  grid_data$order <- if (orderPlots != "none") num_squares else NA

  return(grid_data)
}

#' Create Waffle Plot
#'
#' @description
#' Constructs the waffle plot using the prepared grid data.
#'
#' @param data The grid data prepared for the waffle plot.
#' @param orderPlots Specifies how the plots should be ordered ('ascending', 'descending', or 'none').
#' @param title The title of the waffle plot.
#' @param groupColours A named vector of colours for each category in the plot.
#' @param backgroundColour Colour for the background of the plot.
#' @param backgroundAlpha Alpha value (transparency) for the background.
#' @param borderColour Colour for the border of the tiles in the plot.
#' @param borderWidth Width of the border of the tiles.
#'
#' @return A `ggplot` object representing the waffle plot.
#'
#' @noRd
create_waffle_plot <- function(data,
                               orderPlots,
                               title,
                               groupColours,
                               backgroundColour,
                               backgroundAlpha,
                               borderColour,
                               borderWidth
) {
  # Plot
  p <- ggplot(data,
              aes(x = x, y = y,
                  fill = category)) +
    geom_tile(colour = borderColour,
              linewidth = borderWidth,
              alpha = ifelse(data$category == "zzz", backgroundAlpha, 1)) +
    scale_fill_manual(values = groupColours) +
    labs(title = title) +
    coord_fixed(ratio = 1, clip = "off") +
    facet_wrap(. ~ reorder(label, if (orderPlots == "descending") -order else order)) +
    theme_void() +
    theme(
      plot.margin = margin(0.5, 0.5,
                           0.5, 0.5,
                           "cm"),
      plot.title = element_text(face = "bold",
                                colour = colour_pal("Black96"),
                                size = 12,
                                hjust = 0.5,
                                vjust = 1,
                                margin = margin(0, 0, 0.5, 0, unit = 'cm')),
      strip.text = element_text(face = "bold",
                                colour = colour_pal("Regent Grey"),
                                size = 10),
      legend.position = "none")

  return(p)
}

