# HELPER FUNCTIONS FOR:
# 1) general
# 2) xtabs
# 3) plots

# ============================================================== #
# 1) GENERAL
# Includs:
# test_deparse()
# get_question()
# round_vars()
# append_if_exists()
# list_group()
# check_params()

# ========== #
# Function to test quoted or unquoted argument
test_deparse <- function(x) {
  x.try <- try(x, silent = TRUE)
  if (!inherits(x.try, "try-error") && is.character(x.try))
    x.try
  else
    deparse(substitute(x))
}

# ========== #
# Function to return the original question
get_question <- function(data, var) {
  q <- attr(data[[var]], "label")
  return(q)
}

# ========== #
# Function to round to x decimal places
round_vars <- function(data, decimals) {
  df <- data %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~round(., decimals)
      )
    )
}

# ========== #
# Function to add arguments to a vector
append_if_exists <- function(arguments) {
  a <- unlist(arguments)
  v <- c()
  if (!missing(a)) {
    for (i in c(a)) {
      v <- append(v, i)
    }
  }
  return(v)
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
check_params <- function(data, var, vars, group, groups, weight, groupsPercent,
                         xVar, yVar, meanVar) {
  # `data`: Ensure data is a data frame
  if (!missing(data)) {
    stopifnot("A data frame is required to be parsed through this function." = is.data.frame(data))
  }

  # `var`: Ensure group variable is in data frame
  if (!missing(var)) {
    stopifnot("`var` variable must be a column in `data`." = var %in% names(data))
  }

  # `vars`: Ensure vars is a list and that the list objects are in the data frame
  if (!missing(vars)) {
    stopifnot("`vars` must be a list." = is.list(vars))
    stopifnot("`vars` list must contain column names." = !is.null(names(vars)))
    stopifnot("`vars` list must contain columns from `data`." = names(vars) %in% names(data))
  }

  # `group`: Ensure group variable is in data frame
  if (!missing(group)) {
    stopifnot("`group` variable must be a column in `data`." = group %in% names(data))
  }

  # `groups`: Ensure groups variable is in data frame
  if (!missing(groups)) {
    stopifnot("`groups` variable must be columns in `data`." = groups %in% names(data))
  }

  # `weight`: Ensure weight variable is numeric and in data frame
  if (!missing(weight)) {
    stopifnot("`weight` variable must be a column in `data`." = weight %in% names(data))
    stopifnot("`weight` must be numeric." = is.numeric(data[[weight]]))
  }

  # `groupsPercent`: Ensure groupsPercent variable is in data frame
  if (!missing(groupsPercent)) {
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
}

# ============================================================== #
# 2) XTABS
# Includs:
# xtab_calc()
# xtab_totals()
# xtabs_convert()

# ========== #
# Function to create crosstab and retrive statistics
xtab_calc <- function(data, formula, weight, statistics, type = "xtabs_df") {
  df <- stats::xtabs(
    stats::reformulate(
      formula,
      response = weight
    ), data = data
  )
  x <- summary(df)
  k <- min(length(df), nrow(df))
  n <- x[[2]]
  chisq <- x[[3]]
  cramersv <- sqrt(chisq / (n * (k - 1)))

  p <- round(x[[6]], 3)

  stat <- paste0(formula[1], " x ", formula[2], ": ",
                 "Chisq = ", round(chisq, 3), " | ",
                 "DF = ", x[[4]], " | ",
                 "Cramer's V = ", round(cramersv, 3), " | ",
                 "p-value = ", p
  )

  if (type == "xtabs_df") {
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
xtab_totals <- function(data, row_var, col_var, name) {
  if (missing(name)) {
    # Create column-wise totals
    total <- stats::aggregate(data$Freq, by = list(y = data[, 1]), FUN = sum)
    total$z <- "Total"
    names(total)[names(total) == "x"] <- "Freq"
    names(total)[names(total) == "y"] <- { { row_var } }
    names(total)[names(total) == "z"] <- { { col_var } }
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
    if (format == "csv") {
      formula <- stats::as.formula(~. / sum(.))
    } else
      formula <- stats::as.formula(~. / sum(.) * 100)

    if (format == ("df_long")) {
      df <- transform(data, `Perc` = stats::ave(Freq, data[, 2], FUN = prop.table))
      df <- df %>%
        dplyr::mutate(Perc = as.numeric(df$Perc * 100))
    } else
      df <- data %>%
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::where(is.numeric),
            .fns = formula
          )
        )
  }
  return(df)
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