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
  if (is_political || !missing(assign))
    type <- "discrete_as"

  # For all colour types except for political:
  if (!is_political) {
    if (is_sequential_or_diverging) {
      # Check length of palette requested and if missing, provide maximum value
      n <- if (missing(n)) max(pal$number) else n
      # If sequential or diverging colours, filter by n.
      pal <- pal[pal$number == n, c("value", "colour")]

    } else if (is_categorical) {
      n <- if (missing(n)) max(pal$value) else n
      pal <- pal[pal$value <= n, c("value", "colour")]
    }
    # Rename to  value column to "name"
    names(pal)[names(pal) == "value"] <- "name"

    # For political colour types:
  } else {
    if (is_political) {
      # Select the political palette
      pal <- pal[, c("name", "colour")]
      n <- if (missing(n)) nrow(pal) else n

      if (!missing(assign) && length(assign) > 0) {
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
  if (!missing(assign) && type != "continuous") {
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
