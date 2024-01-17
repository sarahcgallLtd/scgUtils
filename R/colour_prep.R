#' @title Prepare a Named Colour Palette for Data Columns
#' @name colour_prep
#'
#' @description
#' `colour_prep` is a helper function designed to work in tandem with `colour_pal`. It prepares a colour
#' palette for visualising data by assigning colours to unique values across specified columns in a data frame.
#' This function is particularly useful for creating consistent colour mappings in visualisations such as
#' plots and diagrams, where colours are associated with specific data values.
#'
#' For political colour palettes (identified by the "pol" prefix in `pal_name`), `colour_prep` attempts to
#' match political party names from the data frame with corresponding colours from the palette. For example,
#' if a data column contains "Scottish National Party (SNP)", the function will try to match it with the "SNP"
#' entry in the colour palette and assign the appropriate hexadecimal colour, e.g., "#FFF95D" (light yellow).
#'
#' Values that do not match are assigned a default grey colour ("#cccccc"). Users can manually amend these
#' default assignments by modifying the returned list, e.g., `your_saved_colour_palette$default_value <- "#34134a"`.
#'
#' @param data A data frame containing the data for which colours are to be prepared.
#' @param columns A character vector specifying the names of the columns in `data` for which
#'                unique values are to be extracted and assigned colours.
#' @param pal_name The name of the colour palette to use, defaulting to `"catExtended"`.
#'                 This refers to an extended categorical colour palette. Other palettes from
#'                 `colour_pal` can also be specified.
#'
#' @return A named list of hexadecimal colour codes, where each name corresponds to a unique value
#'         found in the specified `columns` of the `data` frame. This list can be directly used for
#'         colour mapping in data visualisations.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   Category1 = c("Scottish National Party (SNP)", "Labour", "Conservative", "Other"),
#'   Category2 = c("An independent candidate", "Invalid_Party_Name", "Other", "Conservative")
#' )
#'
#' # Prepare a colour palette for the unique values in Category1 and Category2
#' colour_prep(df, c("Category1", "Category2"))
#'
#' # See political party colours and compare with the full palette to see how the two names are matched
#' actual <- colour_prep(df, c("Category1", "Category2"), pal_name = "polUK")
#' actual <- stack(actual)
#' expected <- colour_pal("polUK")
#' expected <- stack(expected)
#' colours <- dplyr::left_join(actual, expected, by="values")
#' colours <- stats::setNames(colours, c("colours", "df_name", "palette_name"))
#' print(colours)
#'
#' @export
#' @seealso `colour_pal()`
colour_prep <- function(data,
                        columns,
                        pal_name = "catExtended"
) {
  # Check if specified columns exist in the data frame
  if (!all(columns %in% names(data))) {
    stop("All specified columns must be present in the data frame.")
  }

  # Combine and extract unique values from the specified columns
  combined_values <- unlist(lapply(columns, function(col) as.character(data[[col]])))
  unique_values <- sort(unique(combined_values))

  # Generate colors using colour_pal function with assignment
  palette <- colour_pal(pal_name, assign = unique_values, n = length(unique_values))

  return(palette)
}
