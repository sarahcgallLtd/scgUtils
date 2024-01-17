#' @include utils.R
NULL
#' @title Get Colour Palettes
#' @name colour_pal
#'
#' @description
#' Generates colour palettes for graphs. This function provides a versatile way to retrieve colour
#' schemes from a predefined set of palettes, allowing for either individual colours or ranges of colours
#' based on the specified palette. It supports sequential, diverging, categorical, and political palettes.
#'
#' @param pal_name A character string specifying the name of the desired palette or individual colour.
#'                 Options include:
#'                 - Names of specific colours (e.g., "Jaffa"). Call `colour_display("All") for all colour names.
#'                 - Named political palettes: "polAus", "polNZ", "polUK".
#'                 - Named categorical palettes: "catSimplified" (max n=7), "catExtended" (max n=18).
#'                 - Named sequential palettes (max n=7): "seqGreen", "seqBlue", "seqRed".
#'                 - Named divergent palettes (max n=14): "divRedBlue", "divBlueGreen".
#'                 The function will check the input against the available predefined palettes and colour names.
#'                 If an invalid name is provided, it will return an error.
#'
#' @param n An integer specifying the number of desired colours from the palette.
#'          This is particularly relevant for sequential and diverging palettes.
#'          If not specified, the function defaults to the full length of the specified palette.
#'          A warning is issued if 'n' exceeds the number of available colours in the palette.
#'
#' @param assign An optional character vector representing levels or categories to be associated
#'               with the colours. This is useful for creating named colour vectors where each colour
#'               is assigned a specific label or category. The length of 'assign' should match 'n'.
#'               If not, warnings will be issued for length mismatches.
#'
#' @param type A character string specifying the type of colour palette to return.
#'             Options are "discrete", "discrete_as", or "continuous".
#'             - "discrete": Returns an unnamed vector of colours.
#'             - "discrete_as": Returns a named list of colours based on the 'assign' parameter.
#'             - "continuous": Returns a function for generating colour gradients, applicable
#'               only for sequential or divergent palettes. The default is "discrete".
#'
#' @return Depending on the 'type' parameter, this function returns:
#'         - A vector of colour values ("discrete").
#'         - A named list of colour values ("discrete_as").
#'         - A function to create a gradient of colours ("continuous").
#'
#' @examples
#' # Return full palette
#' colour_pal("catExtended")
#'
#' # Return individual colour
#' colour_pal("Jaffa")
#'
#' # Return palette with 5 colours and assigned levels for each colour
#' colour_pal("divBlueGreen", 5,
#'            c("Very Likely", "Likely", "Neutral", "Unlikely", "Very Unlikely"))
#'
#' @export
#' @seealso `colour_display()`
colour_pal <- function(pal_name,
                       n = NULL,
                       assign = NULL,
                       type = c("discrete", "discrete_as", "continuous")
) {
  # ==============================================================#
  # GET DATA
  # Get internal data file "colours" and take first argument of type if none specified
  colours <- get0("colours", envir = asNamespace("scgUtils"))
  type <- match.arg(type)

  # ==============================================================#
  # INDIVIDUAL COLOUR
  if (pal_name %in% colours$name) {
    # Filter and select colour by name
    pal <- colours[colours$name == pal_name, "colour"]
    return(pal) # Individual colour returned as character string

  } else if (pal_name %in% colours$palette) {
    # Filter palette by name
    pal <- colours[colours$palette == pal_name,]

    # Determine palette category
    is_sequential_or_diverging <- grepl("^(seq|div)", pal_name)
    is_categorical <- grepl("^cat", pal_name)
    is_political <- grepl("^pol", pal_name)

  } else
    # Trigger error if neither colour nor palette can be found
    stop("Palette not found.")

  # ==============================================================#
  # PALETTE COLOURS
  if (is_political || !is.null(assign))
    type <- "discrete_as"

  # For all colour types except for political:
  if (!is_political) {
    if (is_sequential_or_diverging) {
      # Check length of palette requested and if NULL, provide maximum value
      n <- if (is.null(n)) max(pal$number) else n
      # If sequential or diverging colours, filter by n.
      pal <- pal[pal$number == n, c("value", "colour")]

    } else if (is_categorical) {
      n <- if (is.null(n)) max(pal$value) else n
      pal <- pal[pal$value <= n, c("value", "colour")]
    }
    # Rename to  value column to "name"
    names(pal)[names(pal) == "value"] <- "name"

    # For political colour types:
  } else {
    if (is_political) {
      # Select the political palette
      pal <- pal[, c("name", "colour")]
      n <- if (is.null(n)) nrow(pal) else n

      if (!is.null(assign) && length(assign) > 0) {
        assign_colours_list <- stats::setNames(vector("list", length(assign)), assign)

        for (i in seq_along(assign)) {
          # Find the best approximate match using Levenshtein distance
          best_matches <- agrep(assign[i], pal$name, max.distance = 0.1, value = TRUE, ignore.case = TRUE)

          if (length(best_matches) > 0) {
            # If matches are found, use the first one
            best_match_name <- best_matches[1]
            best_match_colour <- pal$colour[which(pal$name == best_match_name)]
            assign_colours_list[[i]] <- best_match_colour
          } else {
            # Try reversed partial match
            reversed_regex_matches <- Filter(function(x) grepl(x, assign[i], ignore.case = TRUE), pal$name)

            if (length(reversed_regex_matches) > 0) {
              # Use the first match from reversed regex match
              reversed_regex_match_name <- reversed_regex_matches[1]
              reversed_regex_match_colour <- pal$colour[which(pal$name == reversed_regex_match_name)]
              assign_colours_list[[i]] <- reversed_regex_match_colour
            } else {
              # Fallback for no matches
              assign_colours_list[[i]] <- "#cccccc"  # Default colour
            }
          }
        }
        return(assign_colours_list)
      }
    }
  }

  # Error if palette returns NULL
  if (nrow(pal) == 0)
    stop("Number of requested colours is greater than what this palette can offer.")

  # ==============================================================#
  # Assign values and prevent continuous type being assigned
  if (!is.null(assign) && type != "continuous") {
    # Resize 'assign' to match 'n'
    if (length(assign) < n) {
      warning("Length of 'assign' is less than 'n'. Assignments will be repeated.")
      assign <- make_unique(assign, n)
    } else if (length(assign) > n) {
      warning("Length of 'assign' is greater than 'n'. Excess names will be ignored.")
      assign <- assign[seq_len(n)]
    }
    # Add expanded 'assign' and colours
    pal <- data.frame(name = assign, colour = pal$colour[seq_len(n)], stringsAsFactors = FALSE)
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
