#' @title Custom Theme for scg Plots
#' @name theme_scg
#'
#' @description
#' `theme_scg` provides a custom ggplot2 theme tailored for scg plots. This theme
#' modifies various aspects of ggplot2 plots, including fonts, colors, panel spacing,
#' and legend formatting, to create a visually consistent and appealing style.
#'
#' @param base_size Base font size for text elements in the plot (default is 10).
#' @param base_font Font family for text elements. See \code{names(pdfFonts())} for
#'   available options. Default is an empty string, which uses the ggplot2 default font.
#'
#' @return A ggplot2 theme object that can be applied to ggplot2 plots.
#'
#' @details
#' The function customizes various components of a ggplot2 plot, including the plot title,
#' axis titles and texts, panel background, grid lines, legend, and more. The theme aims to
#' provide a clean, professional look suitable for scg-style visualizations. Users can specify
#' the base font size and family to further customize the appearance.
#'
#' @examples
#' \dontrun{
#'   ggplot(data = df, aes(x = x, y = y, fill = reorder(group, y))) +
#'     geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9), alpha = 1) +
#'     scale_fill_manual(values = colour_pal("catExtended")) +
#'     labs(title = "Title", fill = y, x = x, y = y) +
#'     theme_scg()
#' }
#'
#' @export
theme_scg <- function(base_size = 10, base_font = "") {
  # FONT
  font <- base_font
  size <- base_size

  # COLOURS
  line.col <- colour_pal("French Grey")
  text.col <- colour_pal("Regent Grey")
  title.col <- colour_pal("Black96")
  subtitle.col <- colour_pal("Black80")

  theme(
    # PLOT
    plot.margin = margin(0.5, 0.5,
                         0.5, 0.5,
                         "cm"),
    plot.background = element_rect(fill = "white",
                                   colour = "white"),
    plot.title = element_text(face = "bold",
                              family = font,
                              colour = title.col,
                              size = size * 1.2,
                              hjust = 0.5,
                              vjust = 1,
                              margin = margin(0, 0, 0.5, 0, unit = 'cm')),
    plot.subtitle = element_text(face = "bold",
                                 family = font,
                                 colour = subtitle.col,
                                 size = size,
                                 vjust = 5,
                                 hjust = 0.5,
                                 margin = margin(0, 0, 0.25, 0, unit = 'cm')),
    plot.caption = element_text(face = "bold",
                                family = font,
                                size = size * 0.8,
                                hjust = 1,
                                vjust = -2,
                                colour = line.col),
    plot.caption.position = "plot",

    # PANEL
    panel.background = element_rect(fill = NA,
                                    colour = NA),
    panel.spacing = unit(0.25, 'cm'),
    panel.border = element_rect(fill = NA,
                                colour = NA),
    panel.grid.major = element_line(colour = line.col,
                                    linewidth = 0.1),
    panel.grid.minor = element_line(colour = line.col,
                                    linewidth = 0.05),

    # AXIS
    axis.title = element_text(face = "bold",
                              family = font,
                              size = size,
                              colour = text.col),
    axis.title.x = element_text(hjust = 0.5,
                                margin = margin(0.5, 0, 0, 0, unit = 'cm')),
    axis.title.y = element_text(vjust = 1,
                                hjust = 0.5),
    axis.text = element_text(face = "plain",
                             family = font,
                             size = size,
                             colour = text.col),
    axis.text.x = element_text(vjust = -0.5),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_line(colour = line.col,
                              linewidth = 0.25),
    axis.ticks.length = unit(0.1, 'cm'),
    axis.line = element_line(colour = line.col,
                             linewidth = 0.25),

    # LEGEND
    legend.background = element_rect(colour = "white",
                                     fill = "white"),
    legend.title = element_text(face = "bold",
                                family = font,
                                size = size,
                                colour = text.col),
    legend.text = element_text(margin = margin(t = 0, r = 10,
                                                 b = 0, l = 0),
                               face = "plain",
                               family = font,
                               size = size,
                               colour = text.col),
    legend.spacing = unit(0.25, 'cm'),
    legend.box.spacing = unit(0.5, 'cm'),
    # legend.key.size = unit(0.35, 'cm'),
    legend.position = "right",

    # STRIP
    strip.background = element_rect(fill = NA,
                                    colour = NA),
    strip.text = element_text(face = "bold",
                              family = font,
                              size = size,
                              colour = text.col),
  )
}
