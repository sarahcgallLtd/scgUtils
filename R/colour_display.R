#' @title Diplay Colour Options
#' @name colour_display
#'
#' @description Visualise colour options
#'
#' @param palette Name of palette to visualise
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
#' # View sequential colour palette with 7 levels
#' colour_display("seqGreen", 7)
#'
#' # View diverging colour palette with 5 levels with assigned values
#' colour_display("divBlueGreen", 5, c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely"))
#'
#' @import ggplot2
#' @export
colour_display <- function(palette, n, assign) {
  if (palette != "All") {
    pal <- match.call(expand.dots = FALSE)
    pal_n <- match(c("n", "assign"), names(pal), 0L)
    pal <- pal[c(1L, pal_n)]
    pal[[1L]] <- quote(colour_pal)
    pal[["pal_name"]] <- palette
    pal[["type"]] <- "discrete_as"
    # Return palette
    pal_col <- eval(pal, parent.frame())

    if (missing(n)) {
      n <- length(pal_col)
    }

  } else {
    colours <- get0("colours", envir = asNamespace("scgUtils"))
    pal_col <- dplyr::filter(colours, stringr::str_detect(palette, "^pol")==FALSE,
                             name != "")
    pal_col <- dplyr::select(pal_col, name, colour) %>% unique()
    n <- nrow(pal_col)
    pal_col <- split(pal_col$colour, pal_col$name)
  }

  # Return contrast test
  con_pal <- contrast_test(pal_col)

  # create data frame
  if (n == 1) {
    df <- data.frame(x= 1:n,
                     y= replicate(n, 100),
                     value=palette)
  } else {
      df <- data.frame(x= 1:n,
                       y= replicate(n, 100),
                       value=names(pal_col))
  }

  p <- ggplot2::ggplot(data=df, aes(x=x, y=y, fill=value, label=value)) +
      geom_bar(stat="identity") +
      scale_fill_manual(values = pal_col) +
      geom_text(colour=ifelse(con_pal==TRUE, "white", "black"),
                position = position_stack(vjust = 0.5)) +
      labs(title = paste0(palette, " (n= ", n, ")")) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 12, hjust=0.5, vjust=-1),
            strip.text = element_text(size=0),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())

  if (palette == "All")
    p <- p + facet_wrap(.~value, scales = "free")

  print(p)
}
