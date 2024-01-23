#' Check Parameters for Various Functions
#'
#' This utility function performs checks on parameters for a variety of functions
#' within the package. It ensures that inputs conform to expected types and
#' values, specifically focusing on data frames and their columns.
#'
#' @param data A data frame that is checked for validity as the primary data source.
#' @param ... Additional parameters to be checked, typically representing columns
#'   in the `data` data frame. These are validated for their presence in `data`.
#' @param vars An optional list of variables to check within the data frame.
#'   Each element of the list should be named and correspond to a column in `data`.
#' @param groups An optional vector specifying group names to be checked against
#'   columns in `data`.
#' @param groupsPercent An optional vector specifying percentage groups that
#'   must be a subset of the groups specified in `groups`.
#'
#' @details
#' The function primarily checks the following:
#'   - `data` is a valid data frame.
#'   - Variables specified in `...`, `vars`, `groups`, and `groupsPercent` are
#'     present as columns in `data`.
#'   - `vars` is a non-empty named list if provided.
#'   - `groupsPercent` is a subset of `groups` if both are provided.
#'   - Specific checks for the presence and type of the `weight` and `meanVar`
#'     columns if they are specified.
#'
#' @return This function does not return a value but stops with an error message
#'   if any check fails.
#'
#' @note This function is used by `crosstab`, `grid_vars`, `grp_freq`, `grp_mean`,
#'   `plot_bigfive`, `plot_binary`, `plot_popn`, `plot_sankey`, and `process_factors`.
#'
#' @noRd
check_params <- function(data, ..., vars = NULL, groups = NULL, groupsPercent = NULL) {
  # Check for common mandatory parameters
  if (!is.data.frame(data)) {
    stop("Parameter `data` is required and must be a data frame.")
  }

  # Function to check if a variable is in the data frame
  check_in_data <- function(name, value, data) {
    if (!all(value %in% names(data))) {
      stop(paste0("`", name, "` must be a column in `data`."))
    }
  }

  # Check optional and less common mandatory parameters
  params <- list(...)
  for (name in names(params)) {
    value <- params[[name]]

    if (!is.null(value)) {
      check_in_data(name, value, data)
      if (name %in% c("weight", "meanVar") && !is.numeric(data[[value]])) {
        stop(paste0("`", name, "` must be numeric."))
      }
    }
  }

  # Special check for vars (list) and groups (vector)
  if (!missing(vars) && !is.null(vars)) {
    if (!is.list(vars) || is.null(names(vars))) {
      stop("`vars` must be a non-empty list with named elements.")
    }
    check_in_data("vars", names(vars), data)
  }

  if (!missing(groups) && !is.null(groups)) {
    check_in_data("groups", groups, data)
  }

  if (!missing(groupsPercent) && !is.null(groupsPercent)) {
    check_in_data("groupsPercent", groupsPercent, data)
    if (!all(groupsPercent %in% groups)) {
      stop("`groupsPercent` variable must be in `groups`.")
    }
  }
}

#' Retrieve Original Question Label
#'
#' This helper function retrieves the original question label associated with a
#' variable in a data frame. If no label attribute is present, it returns the
#' variable name itself.
#'
#' @param data A data frame containing the variable for which the question label
#'   is to be retrieved.
#' @param var The name of the variable within the data frame whose question label
#'   is being sought. This should be a string corresponding to the column name in
#'   `data`.
#'
#' @details
#' The function checks for a `label` attribute on the specified variable in the
#' data frame. If the `label` attribute exists, it is returned as the question
#' label; otherwise, the name of the variable (`var`) is returned.
#'
#' @return Returns the original question label as a string. If no label is
#'   associated with the variable, the variable name is returned.
#'
#' @note This function is used by `compile`, `crosstab`, and `plot_popn`.
#'
#' @noRd
get_question <- function(data, var) {
  question <- if (!is.null(attr(data[[var]], "label"))) attr(data[[var]], "label") else var
  return(question)
}

#' Round Numeric Variables in Data Frame
#'
#' This function rounds all numeric variables in a given data frame to a
#' specified number of decimal places.
#'
#' @param data A data frame containing the numeric variables to be rounded.
#' @param decimals An integer specifying the number of decimal places to which
#'   the numeric variables should be rounded.
#'
#' @details
#' The function identifies all numeric columns in the provided data frame and
#' rounds these columns to the specified number of decimal places. This
#' operation is applied to each numeric column in the data frame.
#'
#' @return Returns a data frame with all numeric variables rounded to the
#'   specified number of decimal places. Non-numeric variables in the data frame
#'   are not modified.
#'
#' @note This function is used by `crosstab`, `grp_frq`, and `grp_mean`.
#'
#' @noRd
round_vars <- function(data, decimals) {
  numeric_cols <- sapply(data, is.numeric)

  data[, numeric_cols] <- round(data[, numeric_cols], digits = decimals)

  return(data)
}

#' Pivot Data from Long to Wide Format
#'
#' This function transforms data from a long format to a wide format, similar
#' to the functionality provided by `tidyr::pivot_wider()`. It rearranges data
#' based on specified variables, creating a wider representation.
#'
#' @param data A data frame in long format that needs to be transformed into
#'   a wide format.
#' @param vars A character vector specifying the variables to use for pivoting.
#'   The first element is used as the identifier variable, and the remaining
#'   elements specify the variables to be spread out.
#'
#' @details
#' The function uses `stats::xtabs` to create a contingency table, spreading
#' the specified `vars` across the columns. It then adjusts the data frame
#' structure to achieve a wide format. The first variable in `vars` is used as
#' the identifier column.
#'
#' @return Returns a data frame in wide format with the first variable in `vars`
#'   as the identifier and other variables spread as columns.
#'
#' @note This function is used by `crosstab`.
#'
#' @importFrom stats reformulate
#' @importFrom stats xtabs
#' @noRd
pivot_wide <- function(data, vars) {
  formula <- stats::reformulate(vars, response = "Freq")
  tmp <- as.data.frame.matrix(stats::xtabs(formula, data), stringsAsFactors = TRUE)

  # Make rownames first column
  tmp <- cbind(rownames(tmp), tmp)

  # Remove index/row names
  rownames(tmp) <- NULL

  # Rename
  names(tmp)[names(tmp) == "rownames(tmp)"] <- vars[1]

  # Return original factor levels
  tmp[, vars[1]] <- factor(tmp[, vars[1]], levels(data[, vars[1]]))

  return(tmp)
}

#' Append Arguments to a Vector
#'
#' This function takes a variable number of arguments and appends them to a
#' vector. If arguments are provided, they are concatenated into a single vector.
#' If no arguments are provided, the function returns `NULL`.
#'
#' @param ... A variable number of arguments.
#'
#' @details
#' The function uses `c(...)` to concatenate all provided arguments into a
#' single vector. If no arguments are provided, the function returns `NULL`.
#' This is useful for dynamically building vectors based on conditional
#' inclusion of elements.
#'
#' @return Returns a concatenated vector of all input arguments if any are
#'   provided. Returns `NULL` if no arguments are provided.
#'
#' @note This function is used by `grid_vars`.
#'
#' @noRd
append_if_exists <- function(...) {
  elements <- c(...)
  if (length(elements) > 0) {
    return(unlist(elements))
  } else {
    return(NULL)
  }
}

#' Create a List of Columns Based on Specified Group
#'
#' This function takes a data frame and a group of column names, and creates a
#' list where each element corresponds to one of the specified columns.
#'
#' @param data A data frame from which columns are selected.
#' @param group A vector of column names to be included in the list.
#'
#' @details
#' The function iterates over the names in `group`, extracts the corresponding
#' column from `data`, and adds it to a list. Each list element is named after
#' the column it represents.
#'
#' @return Returns a list where each element is a column from `data` as specified
#'   in `group`. The names of the list elements correspond to the column names.
#'
#' @note This function is used by `grp_freq` and `grp_mean`.
#'
#' @noRd
list_group <- function(data, group) {
  grp <- list()
  for (x in group) {
    y <- data[, x]
    grp[[x]] <- y
  }
  return(grp)
}

#' Create Radar Chart Coordinates
#'
#' This function sets up the coordinate system for radar charts using ggplot2's
#' polar coordinates. It is a custom coordinate function designed to work
#' seamlessly with ggplot2.
#'
#' @param theta The variable to map onto the angle in the plot.
#'   Can be either "x" or "y".
#' @param start The starting position of the first radar axis in radians,
#'   with 0 being at the top.
#' @param direction The direction in which the radar axes are drawn.
#'   1 for counterclockwise and -1 for clockwise.
#'
#' @details
#' `coord_radar` extends ggplot2's `CoordPolar` to create radar charts. The
#' function allows customization of the radar chart's orientation and direction.
#' It maps the specified `theta` variable onto the angular axes of the plot,
#' and the other variable (either `x` or `y`, whichever is not `theta`) onto
#' the radial axes.
#'
#' @return Returns a ggproto object representing the coordinate system for a
#'   radar chart.
#'
#' @note This function is used by `plot_bigfive`.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 CoordPolar
#' @noRd
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar,
          theta = theta,
          r = ifelse(theta == 'x', 'y', 'x'),
          start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          clip = "off")
}

#' Convert Selected Group Values to Negative in a Data Frame
#'
#' This function multiplies the values in a specified column by -1 for rows where
#' a specified condition based on another column is met. It is typically used
#' for adjusting data for plotting purposes, such as making one group's values
#' negative to distinguish them in a plot.
#'
#' @param data A data frame containing the data to be modified.
#' @param xVar The name of the column in `data` used to check the condition.
#' @param value The value in the `xVar` column that determines which rows are
#'   affected. Rows with this value in `xVar` will have their `column` values
#'   converted to negative.
#' @param column The name of the column in `data` whose values will be
#'   multiplied by -1 if the condition is met.
#'
#' @details
#' The function identifies rows in `data` where `xVar` equals `value` and
#' multiplies the corresponding values in `column` by -1. This is useful in
#' scenarios such as preparing data for plotting, where negative values might be
#' used for visual distinction.
#'
#' @return Returns the modified data frame with selected values in `column`
#'   converted to negative based on the condition in `xVar`.
#'
#' @note This function is used by `plot_popn`.
#'
#' @noRd
convert_neg <- function(data, xVar, value, column) {
  data[data[xVar] == value, column] <- data[data[xVar] == value, column] * -1
  return(data)
}

#' Evaluate Contrast of Color List for Text Legibility
#'
#' This function assesses the brightness of each color in a provided list and
#' determines whether the contrast is suitable for use with text. It's based on
#' the concept that colors with lower brightness are more suitable for text on
#' light backgrounds and vice versa.
#'
#' @param colour_list A list of color values (in hexadecimal or named colors)
#'   to be tested for text contrast.
#'
#' @details
#' The function converts each color in `colour_list` to its RGB values, then
#' applies a formula to determine its brightness. The formula considers standard
#' weights for the red, green, and blue components of the color. A brightness
#' threshold is used to classify whether the color is suitable for text contrast
#' (true if suitable for dark text on light background, false otherwise).
#'
#' @return Returns a data frame with a boolean column `contrast`. Each row
#'   corresponds to a color from `colour_list`, indicating whether it meets the
#'   contrast threshold for text legibility.
#'
#' @note This function is used by `colour_display`.
#'
#' @importFrom grDevices col2rgb
#' @noRd
contrast_test <- function(colour_list) {
  col.list <- data.frame()
  for (x in unname(unlist(colour_list))) {
    contrast <- (sum(grDevices::col2rgb(x) * c(299, 587, 114)) / 1000 < 123)
    col.list <- rbind(col.list, contrast)
  }
  colnames(col.list)[1] <- "contrast"

  return(col.list)
}