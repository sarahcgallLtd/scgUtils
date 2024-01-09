#' @title Diplay Colour Options
#' @name colour_display
#'
#' @description Visualise colour options
#'
#' @param pal_name Name of pal_name to visualise
#' @param n Number of desired colours.
#' @param assign Vector of corresponding levels/cetgories
#'
#' @return a ggplot2 graph
#'
#' @examples
#' # View full pallette
#' colour_display("polUK")
#'
#' # View individual colour
#' colour_display("Jaffa")
#'
#' # View all individual colours
#' colour_display("All")
#'
#' # View sequential colour pal_name with 7 levels
#' colour_display("seqGreen", 7)
#'
#' # View diverging colour pal_name with 5 levels with assigned values
#' colour_display("divBlueGreen", 5, c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely"))
#'
#' @import ggplot2
#' @export
colour_display <- function(pal_name,
                           n,
                           assign
) {
  # ==============================================================#
  # RETURN COLOUR PALETTE
  # If palette is all (categorical - extended)
  if (pal_name != "All") {
    # Match arguments
    pal <- match.call(expand.dots = FALSE)

    # Substitute colour_display() for colour_pal()
    pal[[1L]] <- quote(colour_pal)

    # Add type as "discrete_as" to ensure colour returns names
    pal[["type"]] <- "discrete_as"

    # Evaluate
    pal_col <- eval(pal, parent.frame())

    if (missing(n)) {
      n <- length(pal_col)
    }

  } else {
    #If palette is "All" (categorical - extended)
    # Get internal colours
    colours <- get0("colours", envir = asNamespace("scgUtils"))

    # Filter all categorical colours
    pal_col <- colours[grepl("^pol", colours$palette) == FALSE,]
    pal_col <- pal_col[pal_col$name != "",]

    # Get unique colours
    pal_col <- unique(pal_col[, c("name", "colour")])

    # Count number of colours
    n <- nrow(pal_col)

    # Make list
    pal_col <- split(pal_col$colour, pal_col$name)
  }

  # ==============================================================#
  # CONTRAST TEST
  con_pal <- contrast_test(pal_col)

  # ==============================================================#
  # CREATE DF
  if (n == 1) {
    # for single colours
    df <- data.frame(x = 1:n,
                     y = replicate(n, 100),
                     value = pal_name,
                     label = paste0(pal_name, "\n(", unname(unlist(pal_col)), ")"))
  } else {
    # for palettes
    df <- data.frame(x = 1:n,
                     y = replicate(n, 100),
                     value = names(pal_col),
                     label = paste0(names(pal_col), "\n(", unname(unlist(pal_col)), ")"))
  }

  # ==============================================================#
  # CREATE PLOT
  p <- ggplot2::ggplot(data = df, aes(x = x, y = y, fill = value, label = label)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pal_col) +
    geom_text(colour = ifelse(con_pal == TRUE, "white", "black"),
              position = position_stack(vjust = 0.5)) +
    labs(title = paste0(pal_name, " (n= ", n, ")")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5, vjust = -1),
          strip.text = element_text(size = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

  if (pal_name == "All")
    p <- p + facet_wrap(. ~ value, scales = "free")

  # ==============================================================#
  return(p)
}
