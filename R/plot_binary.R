#' @title Visualise Binary Survey Data with Bar Plots
#' @name plot_binary
#'
#' @description
#' `plot_binary` visualises binary survey responses for specified variables using bar plots.
#' It leverages the `grid_vars` function to create a comparative visual representation,
#' supporting optional grouping and weighting.
#'
#' @param data A data frame containing survey data.
#' @param vars A list mapping original column names to new variable names for visualisation.
#'   For example, `list(Q1a = "Art", Q1b = "Automobiles")`. This parameter is required.
#' @param value The binary response value (e.g., "Yes") to be visualized. Required.
#' @param group An optional variable for comparing responses between different groups.
#' @param weight An optional variable containing weight factors for analysis.
#' @param title The title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param totalColour The colour used for plotting total response percentages (default: French Grey).
#' @param groupColours A vector of colours used for plotting grouped response percentages
#'   (default: an extended categorical palette).
#'
#' @return A ggplot2 bar plot visualising the percentage of the specified binary response
#'   across the chosen variables. The plot includes comparisons for total responses and,
#'   if specified, grouped responses.
#'
#' @details
#' The function first validates the binary nature of the specified variables and then
#' prepares the data for visualisation. The resulting plot shows the proportion of the
#' specified binary response (`value`) for each variable in `vars`. When a `group`
#' variable is provided, the plot includes facet-wrapped comparisons across groups.
#'
#' @examples
#' \dontrun{
#'   # Example: Visualise binary survey responses by gender
#'   vars <- list(Q1a = "Art", Q1b = "Automobiles", Q1c = "Birdwatching")
#'   df <- plot_binary(dataset, vars = vars, value = "Yes", group = "gender", weight = "wgtvar")
#' }
#'
#' @export
plot_binary <- function(data,
                        vars,
                        value,
                        group = NULL,
                        weight = NULL,
                        title = NULL,
                        subtitle = NULL,
                        totalColour = colour_pal("French Grey"),
                        groupColours = colour_pal("catExtended")
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               vars = vars,
               group = group,
               weight = weight)

  # Validate vars
  validate_binary_data(data, vars, value)

  # ==============================================================#
  # PREPARE DATA
  # Data preparation
  prepared_data <- prepare_binary_data(data, vars, value, group, weight)

  # Plotting
  p <- plot_binary_data(prepared_data$total, prepared_data$grouped, group, totalColour)

  # Final plot adjustments
  p <- p +
    # Add labels
    geom_text(aes(hjust = 0),
              size = convert_sizing(11),
              nudge_x = 1,
              colour = colour_pal("Black80")) +

    # Remove axes titles
    labs(title = title,
         subtitle = subtitle,
         x = NULL,
         y = NULL,
         fill = NULL) +

    # Add colours to
    scale_fill_manual(values = groupColours) +

    # Set x axis
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 100),
                       labels = percent_label()) +

    # Turn clip "off"
    coord_cartesian(clip = "off") +

    # Add scg theme
    theme_scg() +

    # Remove horizontal gridlines and legend
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(2, "lines"),
          legend.position = "none")

  # ==============================================================#
  return(p)
}

#' Validate Binary Data in Specified Columns
#'
#' This internal helper function for `plot_binary` checks that specified variables in a data frame
#' contain binary data, with one of the binary values matching the provided value.
#'
#' @param data The data frame to be validated.
#' @param vars A list of variables to check for binary data.
#' @param value The expected binary value to check for in each variable.
#'
#' @return The function does not return a value but stops execution with an error message if the validation fails.
#'
#' @details
#' The function iterates over each specified variable and checks that it contains only two unique values,
#' one of which must match the provided `value`. It ensures the data is suitable for binary data analysis.
#'
#' @noRd
validate_binary_data <- function(data,
                                 vars,
                                 value
) {
  if (!is.list(vars) || is.null(names(vars))) {
      stop("`vars` must be a non-empty list with named elements.")
    }

  if (!is.character(value) || length(value) != 1) {
    stop("`value` must be a single character string.")
  }

  for (i in names(vars)) {
    unique_values <- unique(stats::na.omit(data[[i]]))
    if (!(value %in% unique_values && length(unique_values) == 2)) {
      stop("`vars` variables must contain binary values only, and `value` must be one of these.")
    }
  }
}

#' Prepare Binary Data for Visualisation
#'
#' An internal helper function for `plot_binary` that prepares binary data for visualisation,
#' filtering it based on a specific value and optionally grouping and weighting.
#'
#' @param data The data frame containing binary data.
#' @param vars A list of variables to be visualized.
#' @param value The binary value to filter on.
#' @param group An optional variable for grouping data.
#' @param weight An optional weighting variable.
#'
#' @return A list containing prepared data frames for total and grouped data.
#'
#' @details
#' The function filters the data based on the specified binary `value` and prepares it
#' for visualisation. It handles both total (un-grouped) and grouped data scenarios.
#'
#' @noRd
prepare_binary_data <- function(data,
                                vars,
                                value,
                                group,
                                weight
) {
  # Get grid vars by total
  total <- grid_vars(data, vars, weight = weight)

  # Add total id
  total$id <- "Total"

  # Filter (Reponse = Yes)
  total <- total[total$Response == value, -which(names(total) == "Response")]
  # total <- total[total[, "Response"] == value, -which(names(total) == "Response")]

  grouped <- NULL
  if (!is.null(group)) {
    # Get grid vars by group
    grouped <- grid_vars(data, vars, group, weight)

    # Filter by value
    grouped <- grouped[grouped$Response == value, -which(names(grouped) == "Response")]
  }
  list(total = total, grouped = grouped)
}

#' Plot Binary Data
#'
#' This internal helper function for `plot_binary` creates a ggplot object for visualising
#' binary data, supporting both total and grouped representations.
#'
#' @param total A data frame of total data prepared by `prepare_binary_data`.
#' @param grouped An optional data frame of grouped data prepared by `prepare_binary_data`.
#' @param group The name of the grouping variable, if applicable.
#' @param totalColour The color to be used for the total data bars in the plot.
#'
#' @return A ggplot object representing the binary data as a bar plot.
#'
#' @details
#' Depending on whether `group` is specified, the function creates either a single bar plot
#' (total data) or a facet-wrapped bar plot (grouped data). The total data is always displayed
#' with `totalColour`.
#'
#' @noRd
plot_binary_data <- function(total,
                             grouped,
                             group,
                             totalColour
) {
  if (!is.null(group)) {
    # GROUPED PLOT
    p <- ggplot(data = grouped,
                aes(x = Perc, y = stats::reorder(Question, Perc),
                    fill = !!rlang::ensym(group),
                    label = paste0(round(Perc, 0), "%"))) +

      # Add total layer
      geom_bar(data = total, aes(group = id),
               fill = totalColour, stat = "identity",
               alpha = 0.5) +

      # Add grouped layer
      geom_bar(stat = "identity",
               alpha = 0.8) +

      # Facet by group
      facet_wrap(vars(!!rlang::ensym(group)))
  } else {
    # TOTAL PLOT
    p <- ggplot(data = total,
                aes(x = Perc, y = stats::reorder(Question, Perc),
                    label = paste0(round(Perc, 0), "%"))) +

      # Add total layer
      geom_bar(stat = "identity",
               fill = totalColour, alpha = 0.8)
  }

  return(p)
}
