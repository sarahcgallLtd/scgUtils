#' Visualise Likert Scales with Stacked, Divergent, and Facetted Bar Plots
#' @name plot_likert
#'
#' @description
#' `plot_likert` generates various types of Likert plots (stacked, divergent, or facetted) for visualising
#' survey data. It is versatile in handling different plot types, the inclusion of NET scores, and custom orderings.
#'
#' @param data A data frame containing survey data.
#' @param vars A vector of variables containing Likert responses.
#' @param varLevels A vector or a named list with 'left', 'neutral', and 'right' elements specifying the levels of the Likert scale.
#' @param group An optional grouping variable for creating facetted plots. If specified, `vars` should contain only a single variable.
#' @param weight An optional weighting variable for the survey data.
#' @param type The type of Likert plot: 'stacked', 'divergent', or 'facetted'.
#' @param neutrals Handling of neutral responses in the plot. Can be 'no_change', 'right', or 'exclude'.
#' @param total If TRUE and `group` is specified, includes an option for the total population.
#' @param NET If TRUE, provides a Net Rating Score (positive minus negative responses).
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param order_by Specifies the ordering of responses in the plot.
#' @param colours Colour palette for the variables. Default = colour_pal("divRedBlue").
#' @param legend Position of the legend in the plot ('top', 'right', 'left', 'bottom', 'none').
#' @param nrow Number of rows for the legend, if applicable. Default = 1.
#' @param width Width of the bars in the plot. Default = 0.8.
#' @param ratio Aspect ratio of the plot. Default = 6.
#'
#' @return A `ggplot2` object representing the Likert plot.
#'
#' @examples
#' \dontrun{
#'   data <- survey_data
#'   vars <- c(Q1 = "Category 1", Q2 = "Category 2")
#'   varLevels <- list(left = c("Strongly Disagree", "Disagree"),
#'                     neutral = "Neutral",
#'                     right = c("Agree", "Strongly Agree"))
#'   plot_likert(data, vars, varLevels, type = "divergent")
#' }
#'
#' @export
plot_likert <- function(data,
                        vars, # a vector of variables containing likert responses
                        varLevels = NULL, # vector or a named list with left, neutral, and right elements
                        group = NULL, # if a group, vars must be a single column only
                        weight = NULL, # weighting variable
                        type = c("stacked", "divergent", "facetted"), # plot type
                        neutrals = c("no_change", "right", "exclude"), # position of neutrals (if no "no_change", neutrals must be provided
                        total = FALSE, # if group is provided, the option for the total population will be included if TRUE
                        NET = FALSE, # provide the net rating score (netPos - netNeg)
                        title = NULL,
                        subtitle = NULL,
                        order_by = NULL, # order the plot
                        colours = colour_pal("divRedBlue"), # colour of vars
                        legend = c("top", "right", "left", "bottom", "none"),
                        nrow = 1, # control the number of rows of the legend
                        width = 0.8, # adjust the width of the bars
                        ratio = 6 # adjust the ratio of the plot
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               vars = vars,
               group = group,
               weight = weight
  )

  type <- match.arg(type)
  neutrals <- match.arg(neutrals)
  legend <- match.arg(legend)

  arglist <- check_likert_arguments(vars = vars,
                                    varLevels = varLevels,
                                    group = group,
                                    type = type,
                                    neutrals = neutrals,
                                    total = total,
                                    NET = NET,
                                    order_by = order_by
  )

  neutrals <- arglist$neutrals
  total <- arglist$total
  NET <- arglist$NET
  order_by <- arglist$order_by

  # Reorder resonse variables and check varLevels
  if (!is.null(varLevels)) {
    data <- reorder_response_variables(data, vars, varLevels)
  }

  # ==============================================================#
  # PREPARE DATA
  prepared_data <- grid_vars(data, vars, group = group, weight = weight)


  if (!is.null(varLevels) && is.list(varLevels)) {
    # Add identifier column for positive, negative, and neutral responses
    prepared_data <- add_identifier_column(prepared_data, varLevels)

    if (NET) {
      # Add NET rows
      prepared_data <- add_net_rows(prepared_data)
    }

    if (!is.null(order_by)) {
      prepared_data <- add_order_column(prepared_data, order_by)
    }
  }

  # ==============================================================#
  # PLOT
  p <- switch(type,
              stacked = create_stacked_likert(prepared_data, width),
              divergent = create_divergent_likert(prepared_data),
              facetted = create_facetted_likert(prepared_data))

  # Format base plot
  p <- p +
    scale_fill_manual(values = colours,
                      na.translate = FALSE) +
    coord_fixed(clip = "off", ratio = ratio) +
    labs(title = title,
         subtitle = subtitle,
         y = NULL,
         x = NULL,
         fill = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_scg() +
    theme(legend.position = legend,
          legend.direction = if (legend %in% c("top", "bottom")) "horizontal",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    )

  # Adjust legend
  if (legend %in% c("top", "bottom")) {
    p <- p +
      guides(fill = guide_legend(nrow = nrow))
  }

  # Remove axis if NET column is included
  if (NET) {
    p <- p +
      theme(axis.line.x = element_blank())
  }

  # ==============================================================#
  return(p)
}

#' Check and Validate Likert Plot Arguments
#'
#' This function validates the arguments passed to the `plot_likert` function.
#' It ensures that the combination of arguments is consistent and applicable for the desired plot.
#'
#' @param vars Variables to be used in the Likert plot.
#' @param varLevels Levels of the variables, either as a vector or a named list.
#' @param group Optional grouping variable for facetted plots.
#' @param type The type of Likert plot ('divergent', etc.).
#' @param neutrals Handling of neutral responses in the plot.
#' @param total Whether to include a total count or percentage.
#' @param NET Whether to include a NET (sum of positives minus negatives) calculation.
#' @param order_by Specifies the ordering of responses in the plot.
#'
#' @return A list containing updated and validated parameters.
#' @noRd
check_likert_arguments <- function(vars,
                                   varLevels,
                                   group,
                                   type,
                                   neutrals,
                                   total,
                                   NET,
                                   order_by
) {
  # Check if group is not NULL, then that vars only contains a single variable/column, stop with error
  if (!is.null(group) && length(vars) != 1) {
    stop("When `group` is specified, `vars` should contain only a single variable.")
  }

  # Check if neutrals != "no_change" or if NET == TRUE, varLevels MUST be a list, stop with error
  if ((neutrals != "no_change" ||
    NET ||
    !is.null(order_by)) && !is.list(varLevels)) {
    stop("When `neutrals` is not 'no_change', `NET` is TRUE, or order_by is not NULL, `varLevels` must be a list.")
  }

  # Check if total == TRUE and group = NULL, then make total == FALSE and provide warning that this will be igrnoed as is only for group
  if (isTRUE(total) && is.null(group)) {
    warning("`total` will be ignored because `group` is NULL.")
    total <- FALSE
  }

  # Check if type != "divergent" and neutrals != "no_change", then make neutrals == "no_change" and provide warning that this will be ignored and to suggest a divergent type
  if (type != "divergent" && neutrals != "no_change") {
    warning("`neutrals` will be set to 'no_change' as it is only applicable for the 'divergent' type of plot.")
    neutrals <- "no_change"
  }

  # Check if neutrals == "right" and NET == TRUE, then make NET == FALSE and provide a warning that NET will be ignored as the function only permits one or the other on the right side of the plot
  if (neutrals == "right" && isTRUE(NET)) {
    warning("`NET` will be set to FALSE because `neutrals` is set to 'right'. The function does not permit both.")
    NET <- FALSE
  }

  # Check if order_by is one of the specific options and handle accordingly
  if (!is.null(order_by) && !order_by %in% c("left", "right", "NET")) {
    stop("Invalid value for `order_by`. Choose from 'left', 'right', 'NET', or NULL.")
  }

  # Check if NET is FALSE while user is trying to set order by NET
  if (!is.null(order_by) && order_by == "NET" && !NET) {
    warning("`order_by` set to 'NET' will be ignored because `NET` is FALSE. Setting `order_by` to NULL.")
    order_by <- NULL
  }

  return(list(total = total, neutrals = neutrals, NET = NET, order_by = order_by))
}

#' Reorder Response Variables in Data
#'
#' Reorders the response variables in the dataset as per the specified levels in `varLevels`.
#'
#' @param data The data frame containing the survey data.
#' @param vars The variables to be reordered.
#' @param varLevels A vector or a named list specifying the desired order of variable levels.
#'
#' @return The data frame with reordered factor levels.
#' @noRd
reorder_response_variables <- function(data, vars, varLevels) {
  # Combine all levels into a single vector if varLevels is a list
  if (is.list(varLevels)) {
    if (!all(c("left", "neutral", "right") %in% names(varLevels))) {
      stop("`varLevels` must be a named list with 'left', 'neutral', and 'right' elements.")
    }
    varLevels <- unlist(varLevels)  # Combine list elements into a single vector
  } else if (!is.vector(varLevels)) {
    stop("`varLevels` must be either a vector or a list with 'left', 'neutral', and 'right' elements.")
  }

  # Check for extra and missing levels in varLevels
  for (var_name in names(vars)) {
    data_levels <- levels(data[[var_name]])

    # Check for extra levels in varLevels
    if (!all(varLevels %in% data_levels)) {
      extra_levels <- setdiff(varLevels, data_levels)
      stop(paste0("The following levels in `varLevels` are not present in the column '", var_name, "': ", paste(extra_levels, collapse = ", "), "."))
    }

    # Check for missing levels in varLevels
    if (!all(data_levels %in% varLevels)) {
      missing_levels <- setdiff(data_levels, varLevels)
      stop(paste0("The following levels in the column '", var_name, "' are not specified in `varLevels`: ", paste(missing_levels, collapse = ", "), "."))
    }
  }

  # Apply the specified order to all factors in vars
  data <- dplyr::mutate(data,
                        dplyr::across(
                          tidyr::all_of(names(vars)),
                          ~factor(.x, levels = varLevels)))

  return(data)
}

#' Add Identifier Column to Data
#'
#' Adds an identifier column to the data, classifying responses as 'left', 'neutral', or 'right'.
#'
#' @param data The data frame containing the survey data.
#' @param varLevels Levels of the variables to classify responses.
#'
#' @return The data frame with an added identifier column.
#' @noRd
add_identifier_column <- function(data,
                                  varLevels
) {
  # add Id column
  data <- dplyr::mutate(data, Id = dplyr::case_when(
    Response %in% varLevels$right ~ "right",
    Response %in% varLevels$neutral ~ "neutral",
    Response %in% varLevels$left ~ "left",
    TRUE ~ NA_character_  # Handle any unclassified responses
  ))

  return(data)
}

#' Add NET Rows to Data
#'
#' Calculates the NET (sum of positives minus negatives) for each question and adds it to the data.
#'
#' @param data The data frame containing the survey data.
#'
#' @return The data frame with added NET rows.
#' @noRd
add_net_rows <- function(data) {
  # Group by Question and Id, summarize Freq and Perc
  summary_data <- stats::aggregate(cbind(Freq, Perc) ~ Question + Id, data, sum)

  # Remove Neutrals
  summary_data <- summary_data[summary_data$Id != "neutral",]

  # Initialize NET data frame
  net_data <- data.frame(Question = unique(summary_data$Question), Freq = NA, Perc = NA, stringsAsFactors = FALSE)

  # Calculate NET by subtracting Negative from Positive for each Question
  for (q in net_data$Question) {
    pos_vals <- subset(summary_data, Question == q & Id == "right", select = c("Freq", "Perc"))
    neg_vals <- subset(summary_data, Question == q & Id == "left", select = c("Freq", "Perc"))

    net_data[net_data$Question == q, "Freq"] <- sum(pos_vals$Freq, na.rm = TRUE) - sum(neg_vals$Freq, na.rm = TRUE)
    net_data[net_data$Question == q, "Perc"] <- sum(pos_vals$Perc, na.rm = TRUE) - sum(neg_vals$Perc, na.rm = TRUE)
  }

  # Prepare NET data for merging
  net_data$Response <- NA
  net_data$Id <- "NET"

  # Add NET rows to main data
  final_data <- rbind(data, net_data)

  return(final_data)
}

#' Add Order Column for Plotting
#'
#' Adds a column to the data to facilitate ordering of plot facets based on mean percentage.
#'
#' @param data The data frame containing the survey data.
#' @param order_by The variable by which to order the plot facets.
#'
#' @return The data frame with an added order column.
#' @noRd
add_order_column <- function(data,
                             order_by
) {
  # Calculate the sum/average percentage of left/right/net responses for each question
  order_perc <- stats::aggregate(Perc ~ Question, data[data$Id == order_by,], mean)

  # Merge this information back into your original data
  data <- merge(data, order_perc, by = "Question", suffixes = c("", "_order"))

  return(data)
}

#' Create Stacked Likert Plot
#'
#' Generates a stacked Likert plot from the processed data.
#'
#' @param data The processed data for the plot.
#' @param width Width of the bars in the plot.
#'
#' @return A `ggplot` object representing a stacked Likert plot.
#' @noRd
create_stacked_likert <- function(data,
                                  width
) {
  # Subset data
  plot_data <- data[data$Id != "NET",]

  # Check if 'Perc_order' column exists in the data
  y_aes <- if ("Perc_order" %in% names(plot_data)) stats::reorder(plot_data$Question,
                                                           plot_data$Perc_order) else plot_data$Question

  # Create 100% stacked bar chart
  stacked_plot <- ggplot(plot_data,
                         aes(x = Perc,
                             y = y_aes,
                             fill = Response)
  ) +
    geom_bar(stat = "identity", width = width,
             position = position_stack(reverse = TRUE)) +
    scale_x_continuous(expand = c(0, 0),
                       labels = c("0%", "25%", "50%", "75%", "100%"))


  stacked_plot <- add_net_column(data, width, stacked_plot)

  return(stacked_plot)
}

#' Create Divergent Likert Plot
#'
#' Generates a divergent Likert plot from the processed data.
#'
#' @param data The processed data for the plot.
#'
#' @return A `ggplot` object representing a divergent Likert plot.
#' @noRd
create_divergent_likert <- function(data

) {

}

#' Create Facetted Likert Plot
#'
#' Generates a facetted Likert plot from the processed data.
#'
#' @param data The processed data for the plot.
#'
#' @return A `ggplot` object representing a facetted Likert plot.
#' @noRd
create_facetted_likert <- function(data

) {

}

#' Add NET Column to Likert Plot
#'
#' Adds a NET column to the Likert plot, representing the sum of positives minus negatives.
#'
#' @param data The processed data for the plot.
#' @param width Width of the bars in the plot.
#' @param plot The initial `ggplot` object.
#'
#' @return A `ggplot` object with an added NET column.
#' @noRd
add_net_column <- function(data,
                           width,
                           plot
) {
  if ("NET" %in% data$Id) {
    # Subset NET data
    net_data <- data[data$Id == "NET",]
    # Get position of last factor
    net_label_y_pos <- max(as.numeric(data$Question)) + width

    # Add column using tiles and text
    plot <- plot +
      geom_tile(data = net_data,
                aes(x = 105,
                    y = Question,
                    width = 5,
                    height = width),
                fill = colour_pal("Regent Grey"),
                alpha = 0.3) +
      geom_text(data = net_data,
                aes(x = 105,
                    y = Question,
                    label = ifelse(net_data$Perc >= 0,
                                   paste("+", round(net_data$Perc)),
                                   as.character(round(net_data$Perc)))),
                colour = colour_pal("Black80"),
                size = 3.5,
                hjust = 0.5) +

      # Add the "NET" label with a consistent nudge upwards
      geom_text(aes(x = 105, y = net_label_y_pos, label = "NET"),
                inherit.aes = FALSE, # Prevent inheriting aesthetics
                colour = colour_pal("Regent Grey"),
                fontface = "plain",
                size = 3.75,
                hjust = 0.5) +

      # Manually set axis
      annotate("segment",
               x = 0,
               xend = 100,
               y = 0.4,
               yend = 0.4,
               colour = colour_pal("French Grey"),
               linewidth = 0.25)
  }
  return(plot)
}
