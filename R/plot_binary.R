#' @title Plot Binary Survey Data
#' @name plot_binary
#'
#' @description Visualizes binary survey results using the \code{grid_vars} function.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param vars A list of column names and the renamed variable. This parameter is required.
#' @param value One of two variable names to return (e.g., "Yes"). This variable is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
#' @param weight Variable containing weight factors. This variable is optional.
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
#'             value = "Yes")
#'}
#' @export
plot_binary <- function(data,
                        vars,
                        value,
                        group = NULL,
                        weight = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               vars = vars,
               group = group,
               weight = weight)

  if (missing(vars))
    stop("`vars` is required to be parsed through this function.")

  if (missing(value))
    stop("`value` is required to be parsed through this function.")

  stopifnot("`value` must be a character string." = is.character(value))
  stopifnot("`value` must be a single character string only (e.g., 'Yes')." = length(value) == 1)

  # Check that values are binary and occur in vars
  for (i in names(vars)) {
    x <- as.data.frame(unique(stats::na.omit(data[, i])))
    stopifnot("`value` must be in `vars`." = value %in% x[, 1])
    stopifnot("`vars` variables must contain binary values only (e.g., 'Yes' and 'No')." = nrow(x) == 2)
  }

  # ==============================================================#
  # PREPARE DATA
  # Match arguments
  agg <- match.call(expand.dots = FALSE)

  # Limit to data, vars, and weight
  agg_n <- match(c("data", "vars", "weight"), names(agg), 0L)
  agg <- agg[c(1L, agg_n)]

  # Substitute 'plot_binary' for 'grid_vars'
  agg[[1L]] <- quote(grid_vars)

  # Evaluate
  total <- eval(agg, parent.frame())

  # Add total id
  total$id <- "Total"

  # Filter (Reponse = Yes)
  total <- total[total[, "Response"] == value, -which(names(total) == "Response")]

  if (!is.null(group)) {
    # Add group variable if not missing
    agg[["group"]] <- group

    # Evaluate
    grouped <- eval(agg, parent.frame())

    # Filter by value
    grouped <- grouped[grouped[, "Response"] == value, -which(names(grouped) == "Response")]
  }

  # ==============================================================#
  # PREPARE ATTRIBUTES
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Black80")

  # ==============================================================#
  # PLOT
  # Graph
  if (!is.null(group)) {
    # GROUPED PLOT
    p <- ggplot(data = grouped,
                aes(x = Perc, y = stats::reorder(Question, Perc),
                    fill = !!rlang::ensym(group),
                    label = paste0(round(Perc, 0), "%"))) +

      # Add total layer
      geom_bar(data = total, aes(group = id),
               fill = line.col, stat = "identity", alpha = 0.5) +

      # Add grouped layer
      geom_bar(stat = "identity", alpha = 0.8) +

      # Facet by group
      facet_wrap(vars(!!rlang::ensym(group)))

  } else {
    # TOTAL PLOT
    p <- ggplot(data = total,
                aes(x = Perc, y = stats::reorder(Question, Perc),
                    label = paste0(round(Perc, 0), "%"))) +

      # Add total layer
      geom_bar(stat = "identity", fill = line.col, alpha = 0.8)

  }
  p <- p +
    # Add labels
    geom_text(aes(hjust = 0), nudge_x = 1, colour = text.col) +

    # Remove axes titles
    labs(x = "",
         y = "",
         fill = "") +

    # Add colours to
    scale_fill_manual(values = colour_pal("catExtended")) +

    # Set x axis
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +

    # Add scg theme
    theme_scg() +

    # Remove horizontal gridlines and legend
    theme(panel.grid.major.y = element_blank(),
          legend.position = "none")

  # ==============================================================#
  return(p)
}
