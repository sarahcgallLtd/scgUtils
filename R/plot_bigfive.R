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
  # =========== 1. GET VARIABLES ============ #
  df <- data.frame()
    for (i in big_five) {
      if (!missing(weight)) {
        df1 <- data.frame(Mean = stats::weighted.mean(data[,i], data[,weight]))
      } else {
        df1 <- data.frame(Mean = sum(data[,i])/nrow(data[,i]))
      }
      df1$Metric <- i
      df <- rbind(df, df1)
    }
  df$Group <- "Total"

  if (!missing(group)) {
    grp <- match.call(expand.dots = FALSE)
    grp_n <- match(c("data","group","weight"), names(grp), 0L)
    grp <- grp[c(1L, grp_n)]
    grp[[1L]] <- quote(grp_mean)

    result <- data.frame()
    for (i in big_five) {
      grp[["var"]] <- i
      df1 <- eval(grp, parent.frame())
      df1$Metric <- i
      result <- rbind(result, df1)
    }
    result <- dplyr::rename(result, Group2 = {{ group }})
    result <- rbind(result, result[result[,"Metric"] == big_five[1],])
    result$Metric <- factor(result$Metric, levels = big_five)
  }
  df <- rbind(df, df[df[,"Metric"] == big_five[1],])
  df$Metric <- factor(df$Metric, levels = big_five)

  # =========== 2. CREATE GRID ============ #
  grid <- data.frame()
  for (i in seq(from = 0, to = 100, by = 10)) {
    temp <- data.frame(x = big_five)
    temp <- cbind(temp,data.frame(y = rep(i,nrow(temp))))
    grid <- rbind(grid, temp)
  }
  grid1 <- rbind(grid, grid[grid[,"x"] == big_five[1],])
  grid$x <- factor(grid$x, levels = big_five)
  grid1$x <- factor(grid1$x, levels = big_five)
  grid2 <- data.frame(x = c(0.5, 0.5, 0.5, 0.5),
                      y = c(81, 60.75, 40.75, 20.25),
                      label = c("100","75","50","25"))

  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Regent Grey")
  p <- ggplot2::ggplot(grid1, aes(x=x, y=y, group=y)) +
    geom_path(colour=line.col,
              size=0.15,
              alpha=0.5) +
    geom_path(data=grid1[grid1$y %in% c(50, 100),],
              aes(x=x, y=y, group=y),
              size=0.25) +
    geom_path(data=grid,
              aes(x=x, y=y, group=x),
              colour=line.col,
              size=0.25,
              linetype="dashed") +
    geom_label(data=grid2,
               aes(x = x, y = y, label = label),
               size = 4,
               label.padding = unit(0.0, "lines"),
               label.size = 0,
               label.r = unit(0.0, "lines"),
               fill = "white",
               colour = text.col)

  # =========== 3. PLOT DATA ============ #
  p <- p + geom_point(data=df[df$Group == "Total",],
                      aes(x=Metric, y=Mean, group=Group),
                      colour=text.col) +
    geom_polygon(data=df[df$Group == "Total",],
                 aes(x=Metric, y=Mean, group=Group),
                 alpha=0.3,
                 fill=text.col,
                 colour=text.col)

  if (!missing(group)) {
    p <- p + geom_point(data=result[result$Group2 != "Total",],
                        aes(x=Metric, y=Mean, colour=Group2, fill=Group2, group=Group2)) +
      geom_polygon(data=result[result$Group2 != "Total",],
                   aes(x=Metric, y=Mean, colour=Group2, fill=Group2, group=Group2),
                   alpha=0.3) +
      facet_wrap(~ Group2)
  }
  p <- p +
    scale_fill_manual(values = colour_pal("catExtended"),
                      aesthetics = c("colour", "fill")) +
    scale_y_continuous(limits=c(0,100), expand=c(0,0)) +
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
