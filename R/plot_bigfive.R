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
  check_params(data=data,
               group=group,
               weight=weight)

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
     total1 <- data.frame(Mean = stats::weighted.mean(data[, i], data[, weight]))
    } else {
      # unweighted average
      total1 <- data.frame(Mean = sum(data[, i]) / nrow(data))
   }
    # Add big five name to column
    total1$Metric <- i

    # Add combine data
    total <- rbind(total, total1)
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
      grp[["var"]] <- i
      total1 <- eval(grp, parent.frame())
      total1$Metric <- i
      grouped <- rbind(grouped, total1)
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
  # CREATE GRID
  grid <- data.frame()
  for (i in seq(from = 0, to = 100, by = 10)) {
    temp <- data.frame(x = big_five)
    temp <- cbind(temp, data.frame(y = rep(i, nrow(temp))))
    grid <- rbind(grid, temp)
  }
  grid1 <- rbind(grid, grid[grid[, "x"] == big_five[1],])
  grid$x <- factor(grid$x, levels = big_five)
  grid1$x <- factor(grid1$x, levels = big_five)
  grid2 <- data.frame(x = c(0.5, 0.5, 0.5, 0.5),
                      y = c(81, 60.75, 40.75, 20.25),
                      label = c("100", "75", "50", "25"))

  # ==============================================================#
  # PREPARE ATTRIBUTES

  # Colours
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Regent Grey")

  # Create grid
  p <- ggplot(data = grid1,
              aes(x = x, y = y, group = y)) +
    geom_path(colour = line.col,
              linewidth = 0.15,
              alpha = 0.5) +
    geom_path(data = grid1[grid1$y %in% c(50, 100),],
              aes(x = x, y = y, group = y),
              linewidth = 0.25) +
    geom_path(data = grid,
              aes(x = x, y = y, group = x),
              colour = line.col,
              linewidth = 0.25,
              linetype = "dashed") +
    geom_label(data = grid2,
               aes(x = x, y = y, label = label),
               size = 4,
               label.padding = unit(0.0, "lines"),
               label.size = 0,
               label.r = unit(0.0, "lines"),
               fill = "white",
               colour = text.col)

  # ==============================================================#
  # PLOT DATA
  p <- p +
    geom_point(data = total[total$Group == "Total",],
               aes(x = Metric, y = Mean, group = Group),
               colour = text.col) +
    geom_polygon(data = total[total$Group == "Total",],
                 aes(x = Metric, y = Mean, group = Group),
                 alpha = 0.3,
                 fill = text.col,
                 colour = text.col)

  if (!missing(group)) {
    p <- p +
      geom_point(data = grouped[grouped$Group2 != "Total",],
                 aes(x = Metric, y = Mean, colour = Group2, fill = Group2, group = Group2)) +
      geom_polygon(data = grouped[grouped$Group2 != "Total",],
                   aes(x = Metric, y = Mean, colour = Group2, fill = Group2, group = Group2),
                   alpha = 0.3) +
      facet_wrap(~Group2)
  }
  p <- p +
    scale_fill_manual(values = colour_pal("catExtended"),
                      aesthetics = c("colour", "fill")) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    coord_radar() +
    theme_scg() +
    theme(
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none")

  return(p)
}
