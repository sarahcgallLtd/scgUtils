#' @title Population Profile Plot
#' @name plot_popn
#'
#' @description Visualise the population profile of survey data using age categories and gender.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param age_groups Age group variable. This parameter is required.
#' @param gender Gender variable. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param age_int Actual age vraiable (numeric) to view average age.
#'
#' @return ggplot2 plot of the population structure
#'
#' @examples
#' \dontrun{
#' # Create plot using age groups and age intervals included to return average age
#' plot_popn(dataset,
#'           age_group = "age_categories",
#'           gender = "gender",
#'           weight = "wgtvar",
#'           age_int = "age")
#'}
#' @importFrom magrittr %>%
#' @export
plot_popn <- function(data,
                      age_groups,
                      gender,
                      group,
                      weight,
                      age_int
                      # format=c("survey","aggregated")
) {
  # ====== 1. CHECK DATA & ARGUMENTS ======== #
  # format <- match.arg(format)

  if (!"Male" %in% data[,gender] && !"Female" %in% data[,gender])
    stop("Gender column must include 'Male' and 'Female' levels only.")

  # =========== 2. GET VARIABLES ============ #
  agg <- match.call(expand.dots = FALSE)
  agg_n <- match(c("data","weight"), names(agg), 0L)
  agg <- agg[c(1L, agg_n)]
  agg[[1L]] <- quote(grp_freq)
  agg[["group"]] <- c(age_groups,gender)

  df_total <- eval(agg, parent.frame())
  df_total <- dplyr::mutate(df_total, Perc = round(Freq/sum(Freq) * 100, 2))
  df_total$id <- "Total"

  if (!missing(age_int)) {
    sta <- match.call(expand.dots = FALSE)
    sta_n <- match(c("data","weight"), names(sta), 0L)
    sta <- sta[c(1L, sta_n)]
    sta[[1L]] <- quote(grp_mean)
    sta[["var"]] <- age_int
    sta[["group"]] <- gender
    stats <- eval(sta, parent.frame())
    f_label <- paste0("Female\n", "Average Age = ", round(stats[stats[,"gender"] == "Female","Mean"],1))
    m_label <- paste0("Male\n", "Average Age = ", round(stats[stats[,"gender"] == "Male","Mean"],1))
  }

  if (!missing(group)) {
    grp <- match.call(expand.dots = FALSE)
    grp_n <- match(c("data","weight"), names(grp), 0L)
    grp <- grp[c(1L, grp_n)]
    grp[[1L]] <- quote(grp_freq)
    grp[["group"]] <- c(group,age_groups,gender)
    df_grp <- eval(grp, parent.frame())

    df_grp <- transform(df_grp, Perc = stats::ave(Freq, df_grp[,group],
                                                  FUN = function(x) round(x/sum(x)*100, 2)))
  }

  # ============ 4. ATTRIBUTES ============= #
  if (!missing(group)) {
    x.max <- pmax(max(df_total$Perc),max(df_grp$Perc))
  } else {
    x.max <- max(df_total$Perc)
  }

  # X AXIS
  if (x.max <= 5) {
    axis.mar <- 0.25
    x.lim <- c(-5 - axis.mar, 5 + axis.mar)
    x.br <- c(c(-5, -4, -3, -2, -1, 0) - axis.mar, c(0, 1, 2, 3, 4, 5) + axis.mar)
    x.lbl <- c("5%", "4%", "3%", "2%", "1%", "0%", "0%", "1%", "2%", "3%", "4%", "5%")
    x.max <- 5
  } else if (x.max > 5 && x.max <= 10) {
    axis.mar <- 1
    x.lim <- c(-10 - axis.mar,10 + axis.mar)
    x.br <- c(c(-10, -7.5, -5, -2.5, 0) - axis.mar, c(0, 2.5, 5, 7.5, 10) + axis.mar)
    x.lbl <- c("10%", "7.5%", "5%", "2.5%", "0%", "0%", "2.5%", "5%", "7.5%", "10%")
    x.max <- 10
  } else if (x.max > 10 && x.max <= 15) {
    axis.mar <- 0.25
    x.lim <- c(-15 - axis.mar,15 + axis.mar)
    x.br <- c(c(-15, -12.5, -10, -7.5, -5, -2.5, 0) - axis.mar, c(0, 2.5, 5, 7.5, 10, 12.5, 15) + axis.mar)
    x.lbl <- c("15%", "12.5%", "10%", "7.5%", "5%", "2.5%", "0%", "0%", "2.5%", "5%", "7.5%", "10%", "12.5%", "15%")
    x.max <- 15
  } else if (x.max > 15) {
    axis.mar <- 1
    x.br <- c(c(-30, -25, -20, -15, -10, -5, 0) - axis.mar, c(0, 5, 10, 15, 20, 25, 30) + axis.mar)
    x.lbl <- c("30%", "25%", "20%", "15%", "10%", "5%", "0%", "0%", "5%", "10%", "15%", "20%", "25%", "30%")
    if (x.max <= 20) {
      x.lim <- c(-20 - axis.mar,20 + axis.mar)
      x.max <- 20
    } else if (x.max >20 && x.max <= 25) {
      x.lim <- c(-25 - axis.mar,25 + axis.mar)
      x.max <- 25
    } else {
      x.lim <- c(-30 - axis.mar,30 + axis.mar)
      x.max <- 30
    }
  }

  # Y AXIS
  y.min <- get_lims(df_total, age_groups, "min")
  y.max <- get_lims(df_total, age_groups, "max")
  y.range <- y.max - y.min
  y.mar <- 0.75

  # COLOURS
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Regent Grey")
  colours <- list("Female" = colour_pal("Lilac"),
                  "Male" = colour_pal("Steel Blue"),
                  "Total" = colour_pal("French Grey"))

  # =============== 5. GRAPH =============== #
  if (missing(group)) {
    p <- ggplot(data=df_total,
                aes(y = !! rlang::ensym(age_groups),
                    colour = !! rlang::ensym(gender))) +
      # FEMALE
      geom_linerange(data=df_total[df_total[2] == "Female", ],
                    aes(xmin = axis.mar,
                        xmax = axis.mar+Perc),
                    linewidth=100/y.range) +
      # MALE
      geom_linerange(data=df_total[df_total[2] == "Male", ],
                    aes(xmin = -axis.mar,
                        xmax = -axis.mar+(-Perc)),
                    linewidth=100/y.range)
  } else {
    fact <- length(unique(df_grp[,1]))/2
    p <- ggplot(data=df_grp,
                aes(y = !! rlang::ensym(age_groups),
                    group=!! rlang::ensym(group),
                    colour = !! rlang::ensym(gender))) +
      # FEMALE
      geom_linerange(data=df_total[df_total[2] == "Female", ],
                    aes(xmin = axis.mar,
                        xmax = axis.mar+Perc,
                        group=id),
                     colour = line.col,
                     linewidth=(100/y.range)/fact) +
      geom_linerange(data=df_grp[df_grp[3] == "Female", ],
                     aes(xmin = axis.mar,
                         xmax = axis.mar+Perc),
                     linewidth=(100/y.range)/fact) +
      # MALE
      geom_linerange(data=df_total[df_total[2] == "Male", ],
                    aes(xmin = -axis.mar,
                        xmax = -axis.mar+(-Perc),
                        group=id),
                      colour = line.col,
                      linewidth=(100/y.range)/fact) +
      geom_linerange(data=df_grp[df_grp[3] == "Male", ],
                    aes(xmin = -axis.mar,
                        xmax = -axis.mar+(-Perc),
                        ),
                     linewidth=(100/y.range)/fact) +
      facet_wrap(vars(!! rlang::ensym(group)))
  }

  p <- p +
    geom_segment(aes(x = axis.mar+x.max,
                     xend = axis.mar,
                     y = y.min-y.mar,
                     yend = y.min-y.mar),
                 colour = line.col,
                 linewidth=0.25,
                 arrow = arrow(type='open',
                               length = unit(7.5,'pt'),
                               ends = "first")) +
    geom_segment(aes(x = -axis.mar-x.max,
                     xend = -axis.mar,
                     y = y.min-y.mar,
                     yend = y.min-y.mar),
                 colour = line.col,
                 linewidth=0.25,
                 arrow = arrow(type='open',
                               length = unit(7.5,'pt'),
                               ends = "first")) +
    # LABELS
    geom_label(aes(x = 0,
                   y = !! rlang::ensym(age_groups),
                   label = !! rlang::ensym(age_groups)),
               inherit.aes = FALSE,
               size = 3.5,
               label.padding = unit(0.0, "lines"),
               label.size = 0,
               label.r = unit(0.0, "lines"),
               fill = "white",
               alpha = 0.9,
               colour = text.col) +
    labs(title="Population Structure", x="", y="") +
    scale_colour_manual(values=colours) +
    scale_x_continuous(expand = c(0,0),
                       limits = x.lim,
                       breaks = x.br,
                       labels = x.lbl)+
    scale_y_discrete(expand = c(0, 0)) +
    coord_cartesian(ylim = c(y.min-y.mar, y.max+y.mar), xlim = x.lim, expand=FALSE, clip = 'off') +
    theme_scg() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
    )

  if (!missing(age_int) && missing(group)) {
    p <- p + annotate("text",
                      x=x.max-1,
                      y=y.max,
                      label=f_label,
                      fontface = "bold",
                      colour = colour_pal("Lilac")) +
      annotate("text",
               x=-x.max+1,
               y=y.max,
               label=m_label,
               fontface = "bold",
               colour = colour_pal("Steel Blue"))
  }
  return(p)
}
