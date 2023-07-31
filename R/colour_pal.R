#' @title Get Colour Palettes
#' @name colour_pal
#'
#' @description Generate colours palettes for graphs.
#'
#' @param pal_name Name of desired palette or individual colour.
#' @param n Number of desired colours.
#' @param assign Vector of corresponding levels/cetgories
#' @param type Either "continuous", "discrete", or "discrete_as". Default = "discrete".
#' @return A vector of colours.
#'
#' @examples
#' # Return full palette
#' colour_pal("catExtended")
#'
#' # Return individual colour
#' colour_pal("Jaffa")
#'
#' # Return palette with 5 colours and assigned levels for each colour
#' colour_pal("divBlueGreen", 5, c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely"))
#' @export
#'
colour_pal <- function(pal_name, n, assign, type=c("discrete","discrete_as","continuous")) {
  colours <- get0("colours", envir = asNamespace("scgUtils"))
  type <- match.arg(type)

  # Check if palette or individual colour
  if (pal_name %in% colours$name) {
    pal <- dplyr::filter(colours, name == {{ pal_name }})
    pal <- dplyr::select(pal, colour)
    return(pal$colour)

  } else if (pal_name %in% colours$palette) {
    pal <- dplyr::filter(colours, palette == {{ pal_name }})

  } else
       stop("Palette not found.")

  # Check length of palette requested
  if (missing(n)) {
    n <- length(pal)
  }

  # Prepare palette
  if (stringr::str_detect(pal_name, "^pol")==FALSE) {
    if (stringr::str_detect(pal_name, "^seq")==TRUE || stringr::str_detect(pal_name, "^div")==TRUE)
      pal <- dplyr::filter(pal, number == {{ n }})
    else if (stringr::str_detect(pal_name, "^cat")==TRUE)
      pal <- dplyr::select(pal,value, colour)

    pal <- dplyr::select(pal,value, colour)
    pal <- dplyr::rename(pal, name = value)

  } else if (stringr::str_detect(pal_name, "^pol")==TRUE)
    pal <- dplyr::select(pal,name, colour)

  if (nrow(pal) == 0) {
    stop("Number of requested colours is greater than what this palette can offer.")
  }

  # Assign values
  if (!missing(assign)) {
    values <- data.frame(name = assign)
    pal <- cbind(values, colour=pal$colour)
    type <- "discrete_as"
  }

  # Convert to list
  pal <- split(pal$colour, factor(pal$name, levels=pal$name))

  if (stringr::str_detect(pal_name, "^pol")==TRUE) {
    return(pal)
  } else {
    pal <- switch(type,
                discrete = unname(unlist(pal)),
                discrete_as = pal,
                continuous = grDevices::colorRampPalette(pal)
    )
    return(pal)
  }
}
