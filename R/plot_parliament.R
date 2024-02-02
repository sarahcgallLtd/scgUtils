#' @title Visualise Parliamentary Seats by Party
#' @name plot_parliament
#'
#' @description
#' `plot_parliament` creates a visual representation of a parliament's seat distribution by party.
#' It depicts the number of seats each party holds in a semi-circular parliament layout,
#' with optional addition of a percentage bar to show the popular vote won by each party.
#'
#' @param data A data frame containing survey data, specifically party names and their seat counts.
#' @param partyCol The column in `data` representing the names of political parties.
#' @param seatCol The column in `data` representing the number of seats held by each party.
#' @param percentCol Optional: The column in `data` representing the popular vote won by each party.
#' @param colours A named vector of colours for the parties. If unspecified, default colours from `colour_pal("catExtended")` are used.
#' @param size Size of each seat marker in the plot.
#' @param alpha Transparency level of seat markers.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param legend Position of the legend in the plot. Can be 'none', 'right', 'bottom', 'left', or 'top'.
#' @param majorityLine Boolean; if `TRUE`, adds a line indicating the majority threshold.
#' @param extendLine Length to extend the majority line on both sides, if `majorityLine` is `TRUE`.
#' @param num_rows The number of rows to distribute seats across in the semi-circle. If `NULL`, it's calculated based on total seats.
#' @param rMin Minimum radius for the innermost row in the semi-circle.
#' @param rMax Maximum radius for the outermost row in the semi-circle.
#'
#' @return A `ggplot2` object representing the seat distribution in a parliament, with options to include a percentage bar and a majority line.
#'
#' @examples
#' # Sample usage with fictional data
#'   parliament_data <- data.frame(
#'     Party = c("Party A", "Party B", "Party C"),
#'     Seats = c(120, 80, 50),
#'     Percentage = c(40, 27, 17)
#'   )
#'   plot_parliament(parliament_data, partyCol = "Party", seatCol = "Seats", percentCol = "Percentage")
#'
#' @export
plot_parliament <- function(data,
                            partyCol,
                            seatCol,
                            percentCol = NULL,
                            colours = colour_pal("catExtended"),
                            size = 3,
                            alpha = 1,
                            title = NULL,
                            subtitle = NULL,
                            legend = c("none", "right", "bottom", "left", "top"),
                            majorityLine = FALSE,
                            extendLine = 0.05,
                            num_rows = NULL,
                            rMin = 1,
                            rMax = 2.5
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               partyCol = partyCol,
               seatCol = seatCol,
               percentCol = percentCol)

  legend <- match.arg(legend)

  # ==============================================================#
  # CALCULATE
  # Calculate total number of seats
  total_seats <- sum(data[[seatCol]])

  # Get number of rows
  if (is.null(num_rows))
    num_rows <- round(total_seats / 40, 0)

  # Calculate Coordinates
  coordinates <- calculate_seat_coordinates(total_seats, num_rows, rMin, rMax)

  # Assign parties to seats
  plot_df <- assign_parties_to_seats(data, partyCol, seatCol, coordinates)

  # ==============================================================#
  # PLOT
  # Get legend
  ordered_labels <- get_legend_labels(data, partyCol, seatCol, colours)

  # Create the ggplot
  p <- create_parliament_plot(plot_df, colours, size, alpha, title, subtitle,
                              legend, ordered_labels, majorityLine, extendLine,
                              rMin, rMax)

  if (!is.null(percentCol))
    p <- add_percentage_plot(p, data, percentCol, colours, majorityLine, rMax)

  # ==============================================================#
  return(p)
}

#' Calculate Seat Coordinates for a Parliament Plot
#'
#' This internal helper function for `plot_parliament` calculates the coordinates
#' for seats in a semi-circle arrangement representing a parliament.
#'
#' @param total_seats Total number of seats to be plotted.
#' @param num_rows Number of rows to distribute the seats across.
#' @param rMin Minimum radius for the innermost row.
#' @param rMax Maximum radius for the outermost row.
#'
#' @return A data frame with calculated x and y coordinates for each seat, along with
#'   their corresponding row and angle (theta).
#'
#' @noRd
calculate_seat_coordinates <- function(total_seats,
                                       num_rows,
                                       rMin,
                                       rMax
) {
  # Prepare variables
  radii <- seq(rMax, rMin, length.out = num_rows)
  remaining_seats <- total_seats

  # Function to calculate seats per row and update remaining seats
  calc_row <- function(i, remaining_seats) {
    seats_this_row <- round(remaining_seats * radii[i] / sum(radii[i:num_rows]))
    remaining_seats <- remaining_seats - seats_this_row
    theta <- seq(0, pi, length.out = seats_this_row)
    coords <- data.frame(x = radii[i] * cos(theta),
                         y = radii[i] * sin(theta),
                         row = i,
                         theta = theta)
    list(coords = coords, remaining_seats = remaining_seats)
  }

  # Initialise empty list to store coordinates data frames
  coordinates_list <- vector("list", length = num_rows)

  # Iteratively calculate coordinates per row and update remaining seats
  for (i in 1:num_rows) {
    result <- calc_row(i, remaining_seats)
    coordinates_list[[i]] <- result$coords
    remaining_seats <- result$remaining_seats
  }

  # Combine all coordinates into one data frame
  coordinates <- do.call(rbind, coordinates_list)
  coordinates <- coordinates[order(-coordinates$theta, -coordinates$row),]

  return(coordinates)
}

#' Assign Parties to Seats in a Parliament Plot
#'
#' This internal helper function for `plot_parliament` assigns political parties to
#' the calculated seat coordinates.
#'
#' @param data A data frame containing party information and the number of seats.
#' @param partyCol The column in `data` representing political parties.
#' @param seatCol The column in `data` representing the number of seats for each party.
#' @param coordinates A data frame of seat coordinates calculated by `calculate_seat_coordinates`.
#'
#' @return A data frame of seat coordinates with an additional column for assigned parties.
#'
#' @noRd
assign_parties_to_seats <- function(data,
                                    partyCol,
                                    seatCol,
                                    coordinates
) {
  # Ensure lengths match
  stopifnot(sum(data[[seatCol]]) == nrow(coordinates))

  # assign parties to seats
  assigned_parties <- rep(data[[partyCol]], times = data[[seatCol]])
  coordinates$party <- factor(assigned_parties, levels = data[[partyCol]])

  return(coordinates)
}

#' Get Legend Labels for Parliament Plot
#'
#' An internal helper function for `plot_parliament` that creates labels for the
#' legend, combining party names with their seat counts.
#'
#' @param data A data frame containing party information and the number of seats.
#' @param partyCol The column in `data` representing political parties.
#' @param seatCol The column in `data` representing the number of seats for each party.
#' @param colours A named vector of colours corresponding to each party.
#'
#' @return A vector of labels for the legend with party names and seat counts.
#'
#' @noRd
get_legend_labels <- function(data,
                              partyCol,
                              seatCol,
                              colours
) {
  # If colours is not a named vector, name it using the partyCol values
  if (is.null(names(colours))) {
    # Sort the data by partyCol to match the order of colours if it is an unnamed vector
    data <- data[order(data[[partyCol]]), ]
    names(colours) <- data[[partyCol]]
  } else {
    # Ensure the data is ordered according to the named colours vector
    data <- data[match(names(colours), data[[partyCol]]), ]
  }

  # Check if the partyCol is a factor and its levels match the colours
  if (is.factor(data[[partyCol]])) {
    # If partyCol is a factor, ensure levels are in the same order as the colours
    data[[partyCol]] <- factor(data[[partyCol]], levels = names(colours))
  }

  # Create a vector of labels with party names and their seat counts
  labels_with_counts <- paste0(data[[partyCol]], " (", data[[seatCol]], ")")
  names(labels_with_counts) <- data[[partyCol]]

  # Match the labels with the names in the colours vector
  ordered_labels <- labels_with_counts[names(colours)]

  return(ordered_labels)
}

#' Create Parliament Plot
#'
#' This internal helper function for `plot_parliament` creates the main ggplot object
#' for the parliament plot with given data and aesthetic parameters.
#'
#' @param plot_df Data frame with coordinates and party assignment for each seat.
#' @param colours A named vector of colours corresponding to each party.
#' @param size Size of each seat point in the plot.
#' @param alpha Transparency of each seat point in the plot.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param legend Position of the legend in the plot.
#' @param ordered_labels Labels for the legend in the order of appearance.
#' @param majorityLine Boolean to indicate if a majority line should be added.
#' @param extendLine Length to extend the majority line on both sides.
#' @param rMin Minimum radius for the innermost row of seats.
#' @param rMax Maximum radius for the outermost row of seats.
#' @param text.col Colour for text elements in the plot.
#'
#' @return A ggplot object representing the parliament plot.
#'
#' @noRd
create_parliament_plot <- function(plot_df,
                                   colours,
                                   size,
                                   alpha,
                                   title,
                                   subtitle,
                                   legend,
                                   ordered_labels,
                                   majorityLine,
                                   extendLine,
                                   rMin,
                                   rMax,
                                   text.col = colour_pal("Black80")
) {
  # Create the ggplot
  p <- ggplot(plot_df,
              aes(x = x,
                  y = y)
  ) +
    geom_segment(x = 0, xend = 0, y = rMin - extendLine, yend = rMax + extendLine,
                 linetype = "dashed", linewidth = 0.35,
                 colour = text.col,
                 alpha = ifelse(majorityLine == TRUE, 0.8, 0)) +

    geom_point(aes(colour = party), size = size,
               alpha = alpha) +

    scale_colour_manual(values = colours,
                        labels = ordered_labels) +
    labs(title = title,
         subtitle = subtitle) +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(
      plot.title = element_text(face = "bold",
                                colour = colour_pal("Black96"),
                                size = 12,
                                hjust = 0.5,
                                vjust = 1,
                                margin = margin(0, 0, 0.5, 0, unit = 'cm')),
      plot.subtitle = element_text(face = "bold",
                                   colour = text.col,
                                   size = 11,
                                   vjust = 5,
                                   hjust = 0.5,
                                   margin = margin(0, 0, 0.25, 0, unit = 'cm')),
      legend.title = element_blank(),
      legend.position = legend,
      legend.text = element_text(margin = margin(t = 0, r = 10,
                                                 b = 0, l = 0),
                                 size = 9,
                                 colour = text.col, hjust = 0))

  return(p)
}

#' Add Percentage Bar to Parliament Plot
#'
#' An internal helper function for `plot_parliament` that adds a percentage bar
#' to the parliament plot.
#'
#' @param plot Existing ggplot object representing the parliament plot.
#' @param data Data frame containing percentage information for each party.
#' @param percentCol The column in `data` representing the percentage values.
#' @param colours A named vector of colours corresponding to each party.
#' @param majorityLine Boolean to indicate if a majority line should be added to the percentage bar.
#' @param rMax Maximum radius for the outermost row of seats in the parliament plot.
#' @param text.col Colour for text elements in the plot.
#'
#' @return A modified ggplot object with an added percentage bar.
#'
#' @noRd
add_percentage_plot <- function(plot,
                                data,
                                percentCol,
                                colours,
                                majorityLine,
                                rMax,
                                text.col = colour_pal("Black80")
) {
  # Calculate the relative width of each bar based on the percentage
  data$percent_width <- with(data, data[[percentCol]] / 100 * (2 * rMax))
  data$cumulative_percent <- cumsum(data$percent_width) -
    rMax -
    data$percent_width / 2

  # Calculate gap between plots
  gap <- rMax + (rMax * 0.15)
  height <- rMax * 0.1

  # Add the percentage bars using geom_tile
  p <- plot +
    geom_tile(data = data,
              aes(x = cumulative_percent,
                  y = gap,
                  width = percent_width,
                  height = height,
                  fill = Party),
              colour = "white") +

    geom_segment(x = 0, xend = 0, y = gap - (height / 2), yend = gap + (height / 2),
                 linetype = "solid", linewidth = 0.25,
                 colour = "white",
                 alpha = ifelse(majorityLine == TRUE, 0.8, 0)) +

    geom_text(data = data,
              aes(x = cumulative_percent,
                  y = gap + (height / 1.8),
                  label = paste0(round(Percentage, 1), "%")),
              vjust = -0.5, colour = text.col, size = 3.5) +

    scale_fill_manual(values = colours) +
    guides(fill = "none")

  return(p)
}