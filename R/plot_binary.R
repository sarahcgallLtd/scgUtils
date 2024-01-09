#' @title Plot Binary Survey Data
#' @name plot_binary
#'
#' @description Utilises the function \code{binary_vars} to visualise visualise binary survey results.
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
                        group,
                        weight
) {
  # ==============================================================#
  # CHECK PARAMETERS
  check_params(data = data,
               vars = vars,
               group = group,
               weight = weight)

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
  bin <- match.call(expand.dots = FALSE)

  # Limit to data, vars, and weight
  bin_n <- match(c("data", "vars", "weight"), names(bin), 0L)
  bin <- bin[c(1L, bin_n)]

  # Substitute 'plot_binary' for 'grid_vars'
  bin[[1L]] <- quote(grid_vars)

  # Evaluate
  total <- eval(bin, parent.frame())

  # Add total id
  total$id <- "Total"

  # Filter (Reponse = Yes)
  total <- total[total[, "Response"] == value, -which(names(total) == "Response")]

  if (!missing(group)) {
    # Add group variable if not missing
    bin[["group"]] <- group

    # Evaluate
    grouped <- eval(bin, parent.frame())

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
  if (!missing(group)) {
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
