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
