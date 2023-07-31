#' @title Plot Binary Survey Data
#' @name plot_binary
#'
#' @description Utilises the function \code{binary_vars} to visualise visualise binary survey results.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param vars A list of column names and the renamed variable. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param return_var One of two variable names to return (e.g., "Yes"). This variable is required.
#'
#' @return a ggplot2 graph.
#'
#' @examples
#' \dontrun{
#' # Create list
#' vars <- list(Q1a = "Art",
#'              Q1b = "Automobiles",
#'              Q1c = "Birdwatching")
#' plot_binary(dataset,
#'             vars = vars,
#'             group = "gender",
#'             weight = "wgtvar",
#'             return_var = "Yes")
#'}
#' @export
plot_binary <- function(data,
                        vars,
                        group,
                        weight,
                        return_var
) {
  bin <- match.call(expand.dots = FALSE)
  bin_n <- match(c("data","vars","weight"), names(bin), 0L)
  bin <- bin[c(1L, bin_n)]
  bin[[1L]] <- quote(grid_vars)

  df_total <- eval(bin, parent.frame())
  df_total$id <- "Total"

  # Filter (Reponse = Yes)
  df_total <- df_total[df_total[,"Response"]==return_var,-which(names(df_total) == "Response")]

  if (!missing(group)) {
    bin[["group"]] <- group
    df_group <- eval(bin, parent.frame())
    df_group <- df_group[df_group[,"Response"]==return_var,-which(names(df_group) == "Response")]
  }

  # Graph
  if (!missing(group)) {
    p <- ggplot2::ggplot(data=df_group,
                aes(x = Perc,
                    y = stats::reorder(Question, Perc),
                    group = !! rlang::ensym(group),
                    fill = !! rlang::ensym(group),
                    label = paste0(Perc,"%"))) +
      geom_bar(data=df_total,
               aes(x = Perc,
                   y = stats::reorder(Question, Perc),
                   group = id),
               fill = colour_pal("French Grey"),
               stat = "identity",
               alpha = 0.5) +
      geom_bar(stat = "identity",
               alpha = 0.8) +
      geom_text(data=df_group,
                aes(x = Perc,
                    y = stats::reorder(Question, Perc),
                    group = !! rlang::ensym(group),
                    label = paste0(round(Perc,0),"%")),
                    nudge_x = 5) +
      facet_wrap(vars(!! rlang::ensym(group)))
  } else {
    p <- ggplot2::ggplot(data=df_total,
                aes(x = Perc,
                    y = stats::reorder(Question, Perc),
                    group = id,
                    label = paste0(round(Perc,0),"%"))) +
      geom_bar(stat = "identity",
               fill = colour_pal("French Grey"),
               alpha = 0.8) +
      geom_text(nudge_x = 2.5)
  }
  p <- p + labs(x="",
                y="",
                fill = "") +
    scale_fill_manual(values=colour_pal("catExtended")) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0,100),
                       breaks=c(0,25,50,75,100),
                       labels=c("0%","25%","50%","75%","100%")) +
    theme_scg() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "none")

  return(p)
}
