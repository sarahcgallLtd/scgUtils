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
#'                 * Names of specific colours (e.g., "Jaffa"). Call `colour_display("All") for all colour names.
#'                 * Named political palettes: "polAus", "polNZ", "polUK".
#'                 * Named categorical palettes: "catSimplified" (max n=7), "catExtended" (max n=18).
#'                 * Named sequential palettes (max n=7): "seqGreen", "seqBlue", "seqRed".
#'                 * Named divergent palettes (max n=14): "divRedBlue", "divBlueGreen".
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
#'             * "discrete": Returns an unnamed vector of colours.
#'             * "discrete_as": Returns a named list of colours based on the 'assign' parameter.
#'             * "continuous": Returns a function for generating colour gradients, applicable
#'               only for sequential or divergent palettes. The default is "discrete".
#'
#' @return Depending on the 'type' parameter, this function returns:
#'         * A vector of colour values ("discrete").
#'         * A named list of colour values ("discrete_as").
#'         * A function to create a gradient of colours ("continuous").
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
#' @md
colour_pal <- function(pal_name,
                       n = NULL,
                       assign = NULL,
                       type = c("discrete", "discrete_as", "continuous")
) {
  # ==============================================================#
  # PREPARE VARIABLES
  colours <- get0("colours", envir = asNamespace("scgUtils"))
  type <- match.arg(type)

  # Determine palette category
  is_sequential_or_diverging <- grepl("^(seq|div)", pal_name)
  is_categorical <- grepl("^cat", pal_name)
  is_political <- grepl("^pol", pal_name)

  # Set type for political or assigned colours
  if (is_political || !is.null(assign)) {
    type <- "discrete_as"
  }

  # ==============================================================#
  # INDIVIDUAL COLOUR
  if (pal_name %in% colours$name) {
    return(colours[colours$name == pal_name, "colour"])

  } else if (pal_name %in% colours$palette) {
    pal <- colours[colours$palette == pal_name,]

  } else {
    stop("Palette not found.")
  }

  # ==============================================================#
  # ASSIGNED POLITICAL COLOUR PALETTES
  if (is_political) {
    # Select the political palette
    pal <- pal[, c("name", "colour")]

    # If matching colours to political party name
    if (!is.null(assign) && length(assign) > 0) {
      return(assign_colours(assign, pal))
    }
  }

  # ==============================================================#
  # OTHER COLOUR PALETTES
  # Filter palette
  result <- filter_palette(pal, pal_name, n, assign, is_political,
                           is_sequential_or_diverging, is_categorical)
  pal <- result$pal
  n <- result$n # Update n if it was modified in filter_palette

  # ==============================================================#
  # ASSIGNING NON-POLITICAL PALETTES
  pal <- handle_assignments_and_errors(pal, assign, n, type)

  # ==============================================================#
  # RETURN PALETTE
  return(switch(type,
                discrete = unname(unlist(pal)),
                discrete_as = pal,
                continuous = grDevices::colorRampPalette(pal)
  ))
}

#' Filter Palettes Based on Type
#'
#' An internal helper function for `colour_pal` that filters colour palettes
#' based on their type, such as political, sequential, diverging, or categorical.
#'
#' @param pal The data frame of palettes to filter.
#' @param pal_name The name of the specific palette to filter.
#' @param n The desired number of colours from the palette.
#' @param assign A logical value indicating if the palette should be assigned.
#' @param is_political Boolean indicating if the palette is political.
#' @param is_sequential_or_diverging Boolean indicating if the palette is sequential or diverging.
#' @param is_categorical Boolean indicating if the palette is categorical.
#'
#' @return A list containing the filtered palette `pal` and the number `n`.
#'
#' @details
#' The function applies different filtering criteria based on the palette type.
#' It handles errors if the requested number of colours is not available in the palette.
#'
#' @noRd
filter_palette <- function(pal,
                           pal_name,
                           n,
                           assign,
                           is_political,
                           is_sequential_or_diverging,
                           is_categorical
) {
  # Filter palettes based on type
  if (is_political) {
    # Select the political palette
    n <- if (is.null(n)) nrow(pal) else n
  } else if (is_sequential_or_diverging) {
    # Sequential or Diverging palettes
    n <- if (is.null(n)) max(pal$number) else n
    pal <- pal[pal$number == n, c("value", "colour")]
  } else if (is_categorical) {
    # Categorical palettes
    n <- if (is.null(n)) max(pal$value) else n
    pal <- pal[pal$value <= n, c("value", "colour")]
  }

  # Rename the value column to "name" for consistency
  names(pal)[names(pal) == "value"] <- "name"

  # Error if palette returns NULL
  if (nrow(pal) == 0)
    stop("Number of requested colours is greater than what this palette can offer.")

  return(list(pal = pal, n = n))
}

#' Assign Colours Based on Name Matching
#'
#' This internal helper function for `colour_pal` assigns colours to a given list
#' of names based on the best approximate match from a specified colour palette.
#'
#' @param assign A vector of names for which colours are to be assigned.
#' @param pal A data frame or list containing colour names (`name`) and their
#'   corresponding colour values (`colour`).
#'
#' @details
#' The function iterates over the names in `assign`, attempting to find the best
#' approximate match in `pal` using the Levenshtein distance. If a match is found,
#' the corresponding colour is assigned. If no exact match is found, it attempts
#' a reversed partial match. If no match is found, a default colour is assigned.
#'
#' @return A list where each element corresponds to the colour assigned to the
#'   name in `assign`, with names preserved in the list structure.
#'
#' @noRd
assign_colours <- function(assign,
                           pal
) {
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

#' Handle Assignments and Errors for Colour Palettes
#'
#' This internal helper function for `colour_pal` manages assignments and
#' potential errors in colour palette selection.
#'
#' @param pal The data frame of colour palettes.
#' @param assign A vector of names for assigning specific colours.
#' @param n The number of colours to be assigned.
#' @param type The type of the palette, such as 'continuous'.
#'
#' @return A list of colours matched to the names in `assign`.
#'
#' @details
#' The function adjusts the length of `assign` to match `n`, handling warnings
#' for mismatches. It ensures that colour assignments are appropriate for the
#' palette type and returns the final colour assignments.
#'
#' @noRd
handle_assignments_and_errors <- function(pal,
                                          assign,
                                          n,
                                          type
) {
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

  return(pal)
}

#' Make Duplicate Strings Unique
#'
#' This internal helper function, used by `colour_pal`, ensures that a list of
#' strings are made unique by appending a suffix to duplicates.
#'
#' @param strings A character vector of strings that may contain duplicates.
#' @param n The number of unique strings required.
#'
#' @details
#' The function iterates over the provided strings, appending an increasing
#' numerical suffix to duplicates to ensure uniqueness. It's particularly useful
#' in situations where unique identifiers are needed from potentially repetitive
#' string inputs.
#'
#' @return A character vector of length `n` with unique strings.
#'
#' @noRd
make_unique <- function(strings,
                        n
) {
  result <- character(n)
  count <- rep(1, length(strings))
  names(count) <- strings

  idx <- 1
  while (idx <= n) {
    for (str in strings) {
      suffix <- if (count[str] > 1) paste0("_", count[str]) else ""
      result[idx] <- paste0(str, suffix)
      count[str] <- count[str] + 1
      idx <- idx + 1
      if (idx > n) {
        break
      }
    }
  }
  return(result)
}