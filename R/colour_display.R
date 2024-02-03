#' @include utils.R
NULL
#' @title Diplay Colour Options
#' @name colour_display
#'
#' @description
#' Visualise colour options from a specified palette. This function integrates with `colour_pal`
#' to fetch colour data and uses an internal helper function, `contrast_test`, to ensure text readability on
#' coloured bars. Useful for previewing colour palettes, including individual colours, full palettes, or
#' custom selections.
#'
#' @param pal_name A string specifying the name of the palette to visualise.
#'                 Special option "All" displays all available categorical colours.
#'
#' @param n An integer for the number of desired colours from the palette.
#'          Relevant for sequential and diverging palettes. Defaults to the full length of the palette.
#'          If 'n' exceeds available colours, a warning is issued.
#'
#' @param assign An optional character vector representing levels or categories for the colours.
#'               Used for labelling bars in the plot. Length mismatch with 'n' results in warnings.
#'
#' @param type A character string specifying the type of the colour palette.
#'             Options are "discrete_as" and "continuous". The "continuous" type can only be used
#'             with sequential or divergent palettes. Defaults to "discrete_as".
#'
#' @return A ggplot2 object displaying the colour palette. Each bar represents a colour,
#'         labelled with its name and hexadecimal value. For continuous palettes, a colour gradient
#'         is displayed.
#'
#' @examples
#' # View full pallette
#' colour_display("polUK")
#'
#' # View individual colour
#' colour_display("Jaffa")
#'
#' # View sequential colour palette with 7 levels on a continuous scale
#' colour_display("seqGreen", 7, type="continuous")
#'
#' # View diverging colour pal_name with 5 levels with assigned values
#' colour_display("divBlueGreen", 5, c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely"))
#'
#' @import ggplot2
#' @export
#' @seealso `colour_pal()`
colour_display <- function(pal_name,
                           n = NULL,
                           assign = NULL,
                           type = c("discrete_as", "continuous")
) {
  # ==============================================================#
  # CHECK PARAMS
  type <- match.arg(type)

  if (type == "continuous" && grepl("^(seq|div)", pal_name) == FALSE)
    stop("continuous type can only used with sequential or divergent palettes")

  # ==============================================================#
  # RETURN COLOUR PALETTE
  # If palette is all (categorical - extended)
  if (pal_name != "All") {
    # Get colour palette
    pal_col <- colour_pal(pal_name, n, assign, type)

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

  # Call the helper function to create and format the plot
  p <- create_palette_plot(pal_col, pal_name, n, type)

  return(p)
}

#' Create Palette Plot for Color Visualisation
#'
#' This internal helper function for `colour_display` creates plots to visualise
#' color palettes, handling both discrete and continuous color schemes.
#'
#' @param pal_col A function, list, or vector of colours to be displayed in the plot.
#' @param pal_name The name of the colour palette.
#' @param n The number of colours to display. If `NULL`, it will be determined
#'   based on `pal_col` or `pal_name`.
#' @param type The type of colour palette to display, either 'discrete_as' or
#'   a continuous palette type.
#'
#' @details
#' The function creates a ggplot object displaying the specified colors. For
#' discrete palettes ('discrete_as'), it displays each color as a separate bar.
#' For continuous palettes, it displays a gradient plot. It includes contrast
#' adjustments for text labels based on the brightness of the colors.
#'
#' @return A `ggplot` object visualizing the specified color palette.
#'
#' @noRd
create_palette_plot <- function(pal_col,
                                pal_name,
                                n,
                                type
) {
  if (type == "discrete_as") {
    # Get n if NULL
    if (is.null(n)) n <- length(pal_col)

    # Determine the brightness of the colour for contrast against text colour
    con_pal <- contrast_test(pal_col)

    # Create dummary data
    df <- data.frame(
      x = 1:n,
      y = rep(100, n),
      value = if (n == 1) pal_name else names(pal_col),
      label = if (n == 1) {
        paste0(pal_name, "\n(", unname(unlist(pal_col)), ")")
      } else {
        paste0(names(pal_col), "\n(", unname(unlist(pal_col)), ")")
      }
    )

    # Create plot
    p <- ggplot(data = df, aes(x = x, y = y, fill = value, label = label)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = pal_col) +
      geom_text(colour = ifelse(con_pal == TRUE, "white", "black"),
                position = position_stack(vjust = 0.5)) +
      guides(fill = "none")

  } else {
    # Get n if NULL
    if (is.null(n)) n <- length(colour_pal(pal_name, n, type = "discrete_as"))

    # Create dummy data
    df <- expand.grid(x = 1:n, y = 1:n)
    df$value <- as.vector(t(as.data.frame(stats::toeplitz(1:n))))

    # Create plot for continuous palette
    p <- ggplot(df, aes(x, y, fill = value)) +
      geom_tile() +
      scale_fill_gradientn(colours = pal_col(n)) +
      guides(fill = guide_colourbar(title = NULL,
                                    label = FALSE,
                                    ticks = FALSE))
  }

  # Common formatting for the plot
  p <- p +
    labs(title = paste0(pal_name, " (n= ", n, ")")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5, vjust = -1),
          strip.text = element_text(size = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

  if (pal_name == "All") {
    p <- p + facet_wrap(. ~ value, scales = "free")
  }

  return(p)
}
