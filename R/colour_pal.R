#' @title Get Colour Palettes
#' @name colour_pal
#'
#' @description Generate colours palettes for graphs.
#'
#' @param pal_name Name of desired palette or individual colour.
#' @param n Number of desired colours.
#' @param assign Vector of corresponding levels/cetgories
#' @param type Either "continuous", "discrete" (unnamed), or "discrete_as" (named/assigned). Default = "discrete".
#' @return A vector of colours (discrete) or list of colours with names (discrete_as)
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
colour_pal <- function(pal_name,
                       n,
                       assign,
                       type = c("discrete", "discrete_as", "continuous")
) {
  # ==============================================================#
  # GET DATA
  # Check internal data (sysdata)
  colours <- get0("colours", envir = asNamespace("scgUtils"))

  # Take first argument if none specified
  type <- match.arg(type)

  # ==============================================================#
  # INDIVIDUAL COLOUR
  # First search `name` column:
  if (pal_name %in% colours$name) {
    # Filter colour
    pal <- colours[colours$name == pal_name,]

    # Select colour
    pal <- pal[["colour"]]

    # If individual colour, return colour as string
    return(pal)

    # Then, search `palette` column:
  } else if (pal_name %in% colours$palette) {
    pal <- colours[colours$palette == pal_name,]

    # If not in `name` or `palette`, then stop:
  } else
    stop("Palette not found.")

  # ==============================================================#
  # PALETTE COLOURS
  # For all colour types except for political:
  if (grepl("^pol", pal_name) == FALSE) {
    if (grepl("^seq", pal_name) == TRUE || grepl("^div", pal_name) == TRUE) {
      # Check length of palette requested and if missing, provide maximum value
      if (missing(n)) {
        n <- max(pal$number)
      }

      # If sequential or diverging colours, filter by n.
      pal <- pal[pal$number == n,]
    }
    # Otherwise, if categorical colours, don't.

    # Select value and colour columns
    pal <- pal[, c("value", "colour")]
    # Rename to  value column to "name"
    names(pal)[names(pal) == "value"] <- "name"

    # For political colour types:
  } else {
    # Select name and colour columns
    pal <- pal[, c("name", "colour")]

    # Overwrite type to assigned discrete
    type <- "discrete_as"
  }

  # Error if palette returns NULL
  if (nrow(pal) == 0) {
    stop("Number of requested colours is greater than what this palette can offer.")
  }

  # ==============================================================#
  # Assign values but prevent assigning names to continuous scale
  if (!missing(assign)) {
    # Make vector, data frame
    values <- data.frame(name = assign)

    # Add column to palette
    pal <- cbind(values, colour = pal$colour)

    # Overwrite type to assigned discrete
    type <- "discrete_as"
  }

  # Convert to list
  pal <- split(pal$colour, factor(pal$name, levels = pal$name))

  # ==============================================================#
  # Return palette
  pal <- switch(type,
                discrete = unname(unlist(pal)),
                discrete_as = pal,
                continuous = grDevices::colorRampPalette(pal)
  )
  return(pal)
}
