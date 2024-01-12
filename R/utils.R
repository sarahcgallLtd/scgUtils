# HELPER FUNCTIONS FOR:
# 1) general
# 2) xtabs
# 3) plots

# ============================================================== #
# 1) GENERAL
# Includs:
# get_question()
# round_vars()
# append_if_exists()
# list_group()
# check_params()

# ========== #
# Function to retrieve "label" attribute for each factor column in a data frame
get_factor_labels <- function(data) {
  # Identify factor columns using sapply and is.factor
  factor_cols <- sapply(data, is.factor)

  # Subset the data to include only factor columns
  factor_data <- data[, factor_cols, drop = FALSE]

  # Use lapply to iterate through each factor column and retrieve its "label" attribute
  factor_labels <- lapply(factor_data, function(x) attr(x, "label"))

  # Set names for the resulting list using the names of factor columns in the original data frame
  names(factor_labels) <- names(data)[factor_cols]

  # Return a named list of "label" attributes for each factor column
  return(factor_labels)
}


# ========== #
# Function to get all factor labels
add_attributes <- function(data, attribute_labels) {
  # Iterate through attribute labels
  for (label in names(attribute_labels)) {
    # Check if the label exists in the data frame
    if (label %in% names(data)) {
      # Set the attribute in the new data frame
      attr(data[[label]], "label") <- attribute_labels[[label]]
    }
  }
  # Return data frame
  return(data)
}

# ========== #
# Function to return the original question
get_question <- function(data, var) {
  question <- if (!is.null(attr(data[[var]], "label"))) attr(data[[var]], "label") else var
  return(question)
}

# ========== #
# Function to round to x decimal places
round_vars <- function(data, decimals) {
  numeric_cols <- sapply(data, is.numeric)

  data[, numeric_cols] <- round(data[, numeric_cols], digits = decimals)

  return(data)
}

# ========== #
# Function to pivot wider (equivalent to tidyr::pivot_wider()
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

# ========== #
# Function to add arguments to a vector
append_if_exists <- function(...) {
  elements <- c(...)
  if (length(elements) > 0) {
    return(unlist(elements))
  } else {
    return(NULL)
  }
}

# ========== #
# Function to add list of column names from a group
list_group <- function(data, group) {
  grp <- list()
  for (x in group) {
    y <- data[, x]
    grp[[x]] <- y
  }
  return(grp)
}

# ========== #
# Future updates to simplfiy this:
# Check parameters:
check_params <- function(data, vars, group, groups, weight, groupsPercent,
                         xVar, yVar, meanVar, rowVar, colVar) {
  # `data`: Ensure data is a data frame
  if (!missing(data)) {
    stopifnot("A data frame is required to be parsed through this function." = is.data.frame(data))
  } else {
    stop("A data frame is required to be parsed through this function.")
  }

  # `vars`: Ensure vars is a list and that the list objects are in the data frame
  if (!missing(vars)) {
    stopifnot("`vars` must be a list." = is.list(vars))
    stopifnot("`vars` list must contain column names." = !is.null(names(vars)))
    stopifnot("`vars` list must contain columns from `data`." = names(vars) %in% names(data))
  }

  # `group`: Ensure group variable is in data frame
  if (!missing(group) && !is.null(group)) {
    stopifnot("`group` variable must be a column in `data`." = group %in% names(data))
  }

  # `groups`: Ensure groups variable is in data frame
  if (!missing(groups)) {
    stopifnot("`groups` variable must be columns in `data`." = groups %in% names(data))
  }

  # `weight`: Ensure weight variable is numeric and in data frame
  if (!missing(weight) && !is.null(weight)) {
    stopifnot("`weight` variable must be a column in `data`." = weight %in% names(data))
    stopifnot("`weight` must be numeric." = is.numeric(data[[weight]]))
  }

  # `groupsPercent`: Ensure groupsPercent variable is in data frame
  if (!missing(groupsPercent) && !is.null(groupsPercent)) {
    stopifnot("`groupsPercent` variable must be columns in `data`." = groupsPercent %in% names(data))
    stopifnot("`groupsPercent` variable must be in `groups`." = groupsPercent %in% groups)
  }

  # `age_groups`: Ensure age_groups variable is in data frame
  if (!missing(xVar)) {
    stopifnot("`xVar` variable must be a column in `data`. E.g., 'ageGroups'" = xVar %in% names(data))
  }

  # `gender`: Ensure gender variable is in data frame
  if (!missing(yVar)) {
    stopifnot("`yVar` variable must be a column in `data`. E.g., 'gender'" = yVar %in% names(data))
  }

  # `age_int`: Ensure age_int variable is numeric and in data frame
  if (!missing(meanVar)) {
    stopifnot("`meanVar` variable must be a column in `data`. E.g., 'age'" = meanVar %in% names(data))
    stopifnot("`meanVar` must be numeric." = is.numeric(data[[meanVar]]))
  }

  # `rowVar`: Ensure rowVar variable is in data frame
  if (!missing(rowVar)) {
    stopifnot("`rowVar` variable must be a column in `data`." = rowVar %in% names(data))
  }

  # `colVar`: Ensure colVar variable is in data frame
  if (!missing(colVar)) {
    stopifnot("`colVar` variable must be a column in `data`." = colVar %in% names(data))
  }
}

# ========== #
# Helper function to get bigfive average
get_bigfive_average <- function(data,
                                bigfive,
                                weight = NULL) {
  result <- data.frame()
  for (trait in bigfive) {
    if (!is.null(weight)) {
      # Weighted average
      tmp <- data.frame(Mean = stats::weighted.mean(data[[trait]], data[[weight]]))
    } else {
      # Unweighted average
      tmp <- data.frame(Mean = sum(data[[trait]]) / nrow(data))
    }
    # Add big five name to column
    tmp$Metric <- trait

    # Add combine data
    result <- rbind(result, tmp)
  }

  # Add group id
  result$Group <- "Total"

  # Add grouped to total
  result <- rbind(result, result[result[, "Metric"] == bigfive[1],])

  # Make metrics factors
  result$Metric <- factor(result$Metric, levels = bigfive)

  return(result)

}

# ========== #
# Helper function to get bigfive average by group
get_bigfive_grouped <- function(data,
                                bigfive,
                                group,
                                weight = NULL
) {
  # Evaluate each metric
  result <- data.frame()
  for (trait in bigfive) {
    tmp <- grp_mean(data, trait, group, weight)
    tmp$Metric <- trait
    result <- rbind(result, tmp)
  }

  # Rename group to "Group2"
  names(result)[names(result) == group] <- "Group2"

  # Bind rows
  result<- rbind(result, result[result[, "Metric"] == bigfive[1],])

  # Make metrics factors
  result$Metric <- factor(result$Metric, levels = bigfive)

  return(result)
}


# ============================================================== #
# 2) XTABS
# Includs:
# xtab_calc()
# xtab_totals()
# xtabs_convert()

# ========== #
# Function to create crosstab and retrive statistics
xtab_calc <- function(data, formula, weight = NULL, statistics = FALSE, type = "xtabs_df") {
  # Create frequency matrix
  df <- stats::xtabs(stats::reformulate(formula, response = weight), data = data)

  # Remove columns with all 0s (i.e., factors with no responses)
  x <- summary(df[, colSums(df != 0) > 0])

  # Get statistics from call
  k <- min(length(df), nrow(df))
  n <- x[[2]]
  chisq <- x[[3]]
  cramersv <- sqrt(chisq / (n * (k - 1)))

  p <- round(x[[6]], 3)

  # Combine stats
  stat <- paste0(formula[1], " x ", formula[2], ": ",
                 "Chisq = ", round(chisq, 3), " | ",
                 "DF = ", x[[4]], " | ",
                 "Cramer's V = ", round(cramersv, 3), " | ",
                 "p-value = ", p
  )

  if (type == "xtabs_df") {
    # Convert matrix to data frame
    df <- as.data.frame(df)

    if (statistics == TRUE)
      print(noquote(stat))

  } else if (type == "statistics") {
    df <- data.frame(stat)
    names(df)[names(df) == "stat"] <- formula[1]

  } else if (type == "statistics_df") {
    df <- data.frame(Row_Var = formula[1],
                     Col_Var = formula[2],
                     Size = n,
                     Chisq = round(chisq, 3),
                     DF = x[[4]],
                     CramersV = round(cramersv, 3),
                     p_value = p
    )
  }
  return(df)
}

# ========== #
# Function to add totals and sample size to df
xtab_totals <- function(data, rowVar, colVar, name) {
  if (missing(name)) {
    # Create column-wise totals
    total <- stats::aggregate(data$Freq, by = list(y = data[, 1]), FUN = sum)
    total$z <- "Total"
    names(total)[names(total) == "x"] <- "Freq"
    names(total)[names(total) == "y"] <- rowVar
    names(total)[names(total) == "z"] <- colVar
    df <- dplyr::bind_rows(data, total)
  } else {
    # Create row-wise totals
    df <- janitor::adorn_totals(
      data,
      where = "row",
      name = name
    )
  }
  return(df)
}

# ========== #
# Function to convert frequencies to percent (more options to be added in future)
xtabs_convert <- function(data, convert_to, format) {
  if (convert_to == "percent") {
    if (format == ("df_long")) {
      data <- transform(data, `Perc` = stats::ave(Freq, data[, 2], FUN = prop.table))
      data$Perc <- as.numeric(data$Perc * 100)
    } else {
      if (format == "csv")
        data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) x / sum(x))
      else
        data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) x / sum(x) * 100)
    }
  }
  return(data)
}

# ============================================================== #
# 3) PLOTS
# Includs:
# contrast_test()
# coord_radar()

# ========== #
# Function to determine brightness of colour for contrast against text colour
contrast_test <- function(colour_list) {
  col.list <- data.frame()
  for (x in unname(unlist(colour_list))) {
    contrast <- (sum(grDevices::col2rgb(x) * c(299, 587, 114)) / 1000 < 123)
    col.list <- rbind(col.list, contrast)
  }
  colnames(col.list)[1] <- "contrast"

  return(col.list)
}

# ========== #
# Function to create radar charts
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  ggproto("CoordRadar", CoordPolar,
          theta = theta,
          r = ifelse(theta == 'x', 'y', 'x'),
          start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          clip = "off")
}

# ========== #
# Function to make one group negative on plot
convert_neg <- function(data, xVar, value, column) {
  data[data[xVar] == value, column] <- data[data[xVar] == value, column] * -1
  return(data)
}

# ========== #
# Function to amend text labels in geom_text layers()
add_text <- function(plot, data, column, thresholdLab, nudgeLab, sizeLab, faceLab, colour) {
  p <- plot +
    geom_text(
      aes(hjust = ifelse(abs(!!rlang::ensym(column)) >= thresholdLab, "inward", "outward")),
      size = sizeLab, fontface = faceLab, na.rm = TRUE,
      colour = ifelse(abs(data[, column]) >= thresholdLab, "white", colour),
      nudge_x = dplyr::case_when(
        abs(data[, column]) >= thresholdLab & data[, column] < 0.00 ~ nudgeLab,
        abs(data[, column]) < thresholdLab & data[, column] >= 0.00 ~ nudgeLab,
        .default = -nudgeLab
      ),
      check_overlap = TRUE
    )
  return(p)
}

# TODO: Known issue = when add_text() is used in a facet, the hjust position does not work for smaller negative values

# ========== #
# Function to create circular grid
create_grid <- function(outerLabs,
                        line.col = colour_pal("French Grey"),
                        text.col = colour_pal("Regent Grey")) {
  grid <- data.frame()
  for (i in seq(from = 0, to = 100, by = 10)) {
    tmp <- data.frame(x = outerLabs)
    tmp <- cbind(tmp, data.frame(y = rep(i, nrow(tmp))))
    grid <- rbind(grid, tmp)
  }
  # To ensure grid is fully enclosed when coord_radar is added,
  # duplicate the first outerLabs
  grid <- rbind(grid, grid[grid[, "x"] == outerLabs[1],])
  # Make xVar/outerLabs factor
  grid$x <- factor(grid$x, levels = outerLabs)
  # Add position of
  labels <- data.frame(x = c(0.5, 0.5, 0.5, 0.5),
                       y = c(81, 60.75, 40.75, 20.25),
                       label = c("100", "75", "50", "25"))

  p <- ggplot(data = grid,
              aes(x = x, y = y, group = y)) +
    # Add solid horizonal lines at 10% intervals
    geom_path(colour = line.col,
              linewidth = 0.15,
              alpha = 0.5) +
    # Add distinct solid horizonal lines at 50 and 100 %
    geom_path(data = grid[grid$y %in% c(50, 100),],
              aes(x = x, y = y, group = y),
              linewidth = 0.25) +
    # Add vertical dashed lines
    geom_path(data = unique(grid),
              aes(x = x, y = y, group = x),
              colour = line.col,
              linewidth = 0.25,
              linetype = "dashed") +
    # Add labels
    geom_label(data = labels,
               aes(x = x, y = y, label = label),
               size = 4,
               label.padding = unit(0.0, "lines"),
               label.size = 0,
               label.r = unit(0.0, "lines"),
               fill = "white",
               colour = text.col) +

    # create radar chart
    coord_radar() +

    # Add scg theme
    theme_scg() +

    # Remove all gridlines, titles, axes, and legends
    theme(
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none")

  return(p)
}
