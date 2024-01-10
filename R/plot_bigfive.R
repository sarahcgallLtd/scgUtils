#' @title Big Five Personality Radar Plots
#' @name plot_bigfive
#'
#' @description Visualise the personality trait scores on a radar graph.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param big_five A vector with the columns containing the big five scores. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
#'
#' @return ggplot2 plot of the big five scores
#'
#' @examples
#' \dontrun{
#' # Create plot using age groups
#' plot_bigfive(dataset,
#'              big_five = c("Neuroticism", "Extroversion", "Openness",
#'                           "Agreeableness", "Conscientiousness"),
#'              group = "age_categories",
#'              weight = "wgtvar")
#'}
#' @export
plot_bigfive <- function(data,
                         big_five,
                         group,
                         weight
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               group = group,
               weight = weight)

  # Check that big_five are in df
  stopifnot("`big_five` variable must be a column in `data`." = big_five %in% names(data))

  for (i in big_five) {
    stopifnot("`big_five` must be numeric (0-100)." = is.numeric(data[, i]))
  }

  # ==============================================================#
  # GET AVERAGE OF BIG FIVE BY TOTAL
  total <- data.frame()
  for (i in big_five) {
    if (!missing(weight)) {
      # Weighted average
      tmp <- data.frame(Mean = stats::weighted.mean(data[, i], data[, weight]))
    } else {
      # unweighted average
      tmp <- data.frame(Mean = sum(data[, i]) / nrow(data))
    }
    # Add big five name to column
    tmp$Metric <- i

    # Add combine data
    total <- rbind(total, tmp)
  }

  # Add group id
  total$Group <- "Total"

  # ==============================================================#
  # GET AVERAGE OF BIG FIVE BY GROUP
  if (!missing(group)) {
    # Match arguments
    grp <- match.call(expand.dots = FALSE)

    # Substitute `plot_bigfive` for `grp_mean`
    grp[[1L]] <- quote(grp_mean)

    # Remove `big_five` from arguments
    grp[["big_five"]] <- NULL

    # Evaluate each metric
    grouped <- data.frame()
    for (i in big_five) {
      grp[["meanVar"]] <- i
      tmp <- eval(grp, parent.frame())
      tmp$Metric <- i
      grouped <- rbind(grouped, tmp)
    }

    # Rename group to "Group2"
    names(grouped)[names(grouped) == group] <- "Group2"

    # Bind rows
    grouped <- rbind(grouped, grouped[grouped[, "Metric"] == big_five[1],])

    # Make metrics factors
    grouped$Metric <- factor(grouped$Metric, levels = big_five)
  }

  # Add grouped to total
  total <- rbind(total, total[total[, "Metric"] == big_five[1],])

  # Make metrics factors
  total$Metric <- factor(total$Metric, levels = big_five)

  # ==============================================================#
  # PREPARE ATTRIBUTES & GRID
  # Colours
  text.col <- colour_pal("Regent Grey")

  # Grid
  p <- create_grid(big_five)

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

  if (!missing(group)) {
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
