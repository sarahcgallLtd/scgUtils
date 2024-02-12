#' Visualise Likert Scales with Stacked, Divergent, and Facetted Bar Plots
#' @name plot_likert
#'
#' @description
#' `plot_likert` generates various types of Likert plots (stacked, divergent, or facetted) for visualising
#' survey data, handling different configurations of data presentation including NET scores and custom ordering.
#'
#' @param data A data frame containing survey data.
#' @param vars A vector of variables containing Likert responses. If `group` is specified, `vars` should only contain a single variable.
#' @param varLevels A vector or a named list with 'left', 'neutral', and 'right' elements specifying the levels of the Likert scale.
#' @param group An optional grouping variable for creating facetted plots.
#' @param weight An optional weighting variable for the survey data.
#' @param type The type of Likert plot to generate: 'stacked', 'divergent', or 'facetted'.
#' @param neutrals Handling of neutral responses in the plot, can be 'no_change', 'right', or 'exclude'.
#' @param total If TRUE and `group` is specified, includes an option for the total population alongside group-specific plots.
#' @param NET If TRUE, calculates and includes a Net Rating Score (difference between positive and negative responses).
#' @param addLabels if TRUE, adds % labels to the plot.
#' @param threshold A numeric value to adjust the threshold for labels to be shown.
#' @param title The title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param order_by Specifies how to order responses in the plot. Can be NULL or based on 'left', 'right', or 'NET' responses.
#' @param colours A named vector of colours for plotting variables. Default uses `colour_pal("divRedBlue")`.
#' @param legend Position of the legend ('top', 'right', 'left', 'bottom', or 'none').
#' @param nrow Number of rows in the legend, applicable when `legend` is 'top' or 'bottom'.
#' @param ncol Number of columns to use when facetting the plot by `group`, applicable for 'facetted' plot type.
#' @param width Width of the bars in the plot, applicable to 'stacked' and 'divergent' plot types.
#' @param ratio Aspect ratio of the plot, influencing the spacing and layout of plot elements.
#' @param base_size Base font size for text elements within the plot.
#' @param base_font Base font family for text elements within the plot.
#'
#' @return A `ggplot2` object representing the Likert plot according to the specified parameters.
#'
#' @examples
#' \dontrun{
#'   data <- survey_data
#'   vars <- c("Q1", "Q2")
#'   varLevels <- list(left = c("Strongly Disagree", "Disagree"),
#'                     neutral = "Neutral",
#'                     right = c("Agree", "Strongly Agree"))
#'   plot_likert(data, vars, varLevels, type = "divergent",
#'               group = "DemographicGroup", NET = TRUE, order_by = "right")
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
                        addLabels = FALSE, # add labels
                        threshold = 0, # threshold
                        title = NULL, # add title
                        subtitle = NULL, # add subtitle
                        order_by = NULL, # order the plot
                        colours = NULL, # colours
                        legend = c("top", "right", "left", "bottom", "none"),
                        nrow = 1, # control the number of rows of the legend
                        ncol = 3, # control the number of columns in facets
                        width = 0.8, # adjust the width of the bars
                        ratio = 6, # adjust the ratio of the plot
                        base_size = 10, # base font size
                        base_font = "" # base font style
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
  group <- arglist$group

  # Reorder resonse variables and check varLevels
  if (!is.null(varLevels)) {
    data <- reorder_response_variables(data, vars, varLevels)
  }

  # Return plot logic for type of plot to create based on vars and group arguments
  logic <- if (is.null(group)) "standard" else if (length(vars) > 1) "group_facet" else "group_y"

  # ==============================================================#
  # PREPARE DATA
  prepared_data <- grid_vars(data, vars, group = group, weight = weight)

  if (total) {
    total_data <- grid_vars(data, vars, weight = weight)
    total_data[[group]] <- "Total"
    prepared_data <- rbind(prepared_data, total_data)
  }

  # Colours
  if (is.null(colours)) {
    n <- length(unique(prepared_data$Response))
    colours <- colour_pal("divRedBlue", n = n)
  }

  if (!is.null(varLevels) && is.list(varLevels)) {
    # Add identifier column for positive, negative, and neutral responses
    prepared_data <- add_identifier_column(prepared_data, varLevels)

    if (NET) {
      # Add NET rows
      prepared_data <- add_net_rows(prepared_data, group)
    }

    if (!is.null(order_by)) {
      prepared_data <- add_order_column(prepared_data, group, order_by, logic)
    }
  }

  # ==============================================================#
  # PLOT
  # Font size
  geom_size <- base_size + 1
  if (type == "facetted") {
    ratio <- ratio * 2
  }

  # Get plot type
  p <- switch(type,
              stacked = create_stacked_likert(prepared_data, group, addLabels, threshold,
                                              width, ncol, geom_size, logic),
              divergent = create_divergent_likert(prepared_data, group, addLabels, threshold,
                                                  width, ncol, geom_size, logic),
              facetted = create_facetted_likert(prepared_data, group, addLabels, threshold,
                                                width, geom_size, logic))

  # Format base plot
  p <- p +
    scale_fill_manual(values = colours,
                      na.translate = FALSE) +
    coord_fixed(clip = "off", ratio = ratio) +
    labs(title = title,
         subtitle = subtitle,
         y = NULL,
         x = NULL,
         colour = NULL,
         fill = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_scg(base_size, base_font) +
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

  # Remove x-axis if addLabels = TRUE
  if (addLabels) {
    p <- p +
      guides(colour = "none") +
      scale_colour_manual(values = contrast_test(colours)) +
      theme(axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()
      )
  }

  # ==============================================================#
  return(p)
}

#' Validates Arguments for Likert Plot Function
#'
#' Ensures that the arguments provided to the `plot_likert` function are consistent
#' and compatible with the intended plot configuration.
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
#' @return A list containing validated parameters.
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

  # Check if type is facetted, group is supplied, and vars >1
  if (type == "facetted" && !is.null(group) && length(vars) > 1) {
    warning("`group` will be ignored because `type` is 'facetted'. Setting `group` to NULL")
    group <- NULL
  }

  # Check if type is facetted, and NET = TRUE
  if (type == "facetted" && isTRUE(NET)) {
    warning("`NET` will be ignored because `type` is 'facetted'. Setting `NET` to FALSE")
    NET <- FALSE
  }

  return(list(total = total, neutrals = neutrals, NET = NET, order_by = order_by, group = group))
}

#' Reorders Response Variables Based on Specified Levels
#'
#' Adjusts the order of response variable levels in the dataset according to the
#' levels specified in `varLevels`.
#'
#' @param data Dataset containing the variables to be reordered.
#' @param vars Variables whose levels are to be reordered.
#' @param varLevels A vector or a named list specifying the desired order of variable levels.
#'
#' @return Dataset with reordered factor levels for specified variables.
#' @noRd
reorder_response_variables <- function(data,
                                       vars,
                                       varLevels
) {
  vars <- if (is.list(vars)) names(vars) else vars

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
  for (var_name in vars) {
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
                          tidyr::all_of(vars),
                          ~factor(.x, levels = varLevels)))

  return(data)
}

#' Adds Identifier Column to Dataset
#'
#' Classifies responses into 'left', 'neutral', or 'right' categories based on
#' `varLevels` and adds this classification as a new column.
#'
#' @param data Dataset containing the survey responses.
#' @param varLevels Named list specifying the levels associated with 'left', 'neutral', and 'right'.
#'
#' @return Dataset with an added identifier column.
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

#' Adds NET Rows to Data
#'
#' Computes the NET score (difference between positive and negative responses)
#' for each question and adds these scores as new rows in the dataset.
#'
#' @param data Dataset containing the survey responses.
#'
#' @return Dataset augmented with NET score rows.
#' @noRd
add_net_rows <- function(data,
                         group
) {
  # Determine the columns to group by
  group_cols <- append_if_exists("Question", group)

  # Determine formula
  agg_formula <- stats::as.formula(paste0("cbind(Freq, Perc) ~ Question + Id", if (!is.null(group)) paste0("+", group) else NULL))

  # Aggregate data and remove neutrals from data
  agg_data <- stats::aggregate(agg_formula, data = data, subset = Id != "neutral", FUN = sum, na.rm = TRUE)

  # Create a net data frame
  net_data <- data.frame(unique(agg_data[group_cols]), Response = NA,
                         Freq = NA, Perc = NA, Id = "NET",
                         stringsAsFactors = TRUE)

  # Loop through unique combinations of groupings to calculate NET values
  for (i in seq_len(nrow(net_data))) {
    subset_condition <- as.logical(Reduce("&", lapply(group_cols, function(col) agg_data[[col]] == net_data[i, col])))
    right_vars <- agg_data[subset_condition & agg_data$Id == "right",]
    left_vars <- agg_data[subset_condition & agg_data$Id == "left",]
    net_data$Freq[i] <- sum(right_vars$Freq, na.rm = TRUE) - sum(left_vars$Freq, na.rm = TRUE)
    net_data$Perc[i] <- sum(right_vars$Perc, na.rm = TRUE) - sum(left_vars$Perc, na.rm = TRUE)
  }

  # Add NET rows to main data
  final_data <- rbind(data, net_data)

  return(final_data)
}

#' Adds Order Column for Faceting
#'
#' This function calculates an order metric based on the mean percentage of responses according to a specified criterion. It then adds this metric as a new column to the dataset to facilitate ordered faceting in plots. The function adapts to different plotting logics, including standard, group faceting, and grouping by a specific variable for y-axis ordering.
#'
#' @param data Dataset containing the survey responses.
#' @param group (Optional) The grouping variable used in faceted plots. This parameter influences how ordering calculations are performed, especially in the context of grouped or facetted visualizations.
#' @param order_by The variable based on which the order of facets or plot elements is determined. This typically refers to a specific response category whose average percentage is used for ordering.
#' @param logic A character string indicating the plotting logic to be applied. This affects the aggregation formula and the merge process. Valid options are 'standard', 'group_facet', and 'group_y'. The 'standard' logic applies when no grouping is used, 'group_facet' applies when facetting by groups, and 'group_y' is used when grouping affects the y-axis ordering directly.
#'
#' @return A dataset with an added order column (`_order` suffix), facilitating the ordered arrangement of plot facets or elements.
#'
#' @details
#' The `add_order_column` function enhances data preparation for Likert and other survey plots by enabling ordered visualization based on response averages. It supports varying levels of complexity in plot design, accommodating simple, grouped, and facetted layouts.
#'
#'
#' @noRd
add_order_column <- function(data,
                             group,
                             order_by,
                             logic
) {
  # Calculate the sum/average percentage of left/right/net responses for each question
  order_formula <- switch(logic,
                          standard = "Perc ~ Question",
                          group_facet = paste0("Perc ~ Question + ", group),
                          group_y = paste0("Perc ~ ", group)
  )
  order_perc <- stats::aggregate(stats::as.formula(order_formula), data[data$Id == order_by,], mean)

  # Merge this information back into your original data
  byCols <- switch(logic,
                   standard = "Question",
                   group_facet = c("Question", group),
                   group_y = group
  )
  data <- merge(data, order_perc, by = byCols, suffixes = c("", "_order"))

  return(data)
}

#' Create Base Likert Plot
#'
#' @description
#' `create_base_likert_plot` constructs the foundational elements of a Likert plot,
#' including ordered responses and applied aesthetics for further customization into
#' stacked, divergent, or facetted Likert plots.
#'
#' @param data A processed data frame appropriate for Likert plotting.
#' @param group An optional variable used to group data, influencing the y-axis and facet structure.
#' @param threshold A numeric value determining the minimum percentage for label visibility on the plot.
#' @param width The width of bars in the plot.
#' @param logic A character string indicating the plotting logic: 'standard', 'group_facet', or 'group_y',
#' affecting the plot's structure and labeling.
#'
#' @return A `ggplot2` object representing the base of a Likert plot, ready for further customization.
#'
#' @noRd
create_base_likert_plot <- function(data,
                                    group,
                                    threshold,
                                    width,
                                    logic
) {
  # Subset data
  plot_data <- if ("Id" %in% names(data)) data[data$Id != "NET",] else data

  # Determine the variable to use based on 'logic'
  y_var <- if (logic == "group_y") plot_data[[group]] else plot_data$Question

  # Check if 'Perc_order' column exists in the data
  y_aes <- if ("Perc_order" %in% names(plot_data)) stats::reorder(y_var, plot_data$Perc_order) else y_var

  # Create base bar chart
  base_plot <- ggplot(plot_data,
                         aes(x = Perc,
                             y = y_aes,
                             fill = Response,
                             label = ifelse(Perc > threshold, sprintf("%.0f%%", Perc), ""))
  ) +
    geom_bar(stat = "identity", width = width,
             position = position_stack(reverse = TRUE)) +
    scale_x_continuous(expand = c(0, 0),
                       labels = percent_label())

  return(base_plot)
}

#' Create Stacked Likert Plot
#'
#' Enhances the base Likert plot to produce a stacked visualization, aligning
#' responses proportionally within the same axis space for direct comparison across categories.
#'
#' @inheritParams create_base_likert_plot
#'
#' Additional parameters and customization options can be added as needed.
#'
#' @return A `ggplot2` object representing a stacked Likert plot.
#'
#' @noRd
create_stacked_likert <- function(data,
                                  group,
                                  addLabels,
                                  threshold,
                                  width,
                                  ncol,
                                  geom_size,
                                  logic
) {
  # Create 100% stacked bar chart
  stacked_plot <- create_base_likert_plot(data, group, threshold, width, logic)

  # Add labels
  if (addLabels) {
    stacked_plot <- stacked_plot +
      geom_text(aes(colour = Response),
                position = position_stack(vjust = .5, reverse = TRUE),
                size = convert_sizing(geom_size))
  }

  # Add NET column
  stacked_plot <- add_net_column(data, width, stacked_plot,
                                 group, geom_size, logic, addLabels)

  # Add facet group
  if (logic == "group_facet") {
    stacked_plot <- stacked_plot +
      facet_wrap(~.data[[group]], ncol = ncol)
  }

  return(stacked_plot)
}

#' Create Divergent Likert Plot
#'
#' Transforms the base Likert plot into a divergent representation, emphasizing
#' the polarity of responses by positioning them divergently from a central axis.
#'
#' @inheritParams create_base_likert_plot
#'
#' Additional parameters and customization options can be added as needed.
#'
#' @return A `ggplot2` object representing a divergent Likert plot.
#'
#' @noRd
create_divergent_likert <- function(data,
                                    group,
                                    addLabels,
                                    threshold,
                                    width,
                                    ncol,
                                    geom_size,
                                    logic

) {
  # Subset data
  plot_data <- if ("Id" %in% names(data)) data[data$Id != "NET",] else data

  # Determine the variable to use based on 'logic'
  y_var <- if (logic == "group_y") plot_data[[group]] else plot_data$Question

  # Check if 'Perc_order' column exists in the data
  y_aes <- if ("Perc_order" %in% names(plot_data)) stats::reorder(y_var, plot_data$Perc_order) else y_var

  # Create 100% stacked bar chart
  stacked_plot <- ggplot(plot_data,
                         aes(x = Perc,
                             y = y_aes,
                             fill = Response,
                             label = ifelse(Perc > threshold, sprintf("%.0f%%", Perc), ""))
  ) +
    geom_bar(stat = "identity", width = width,
             position = position_stack(reverse = TRUE)) +
    scale_x_continuous(expand = c(0, 0),
                       labels = percent_label())
}

#' Create Facetted Likert Plot
#'
#' Adapts the base Likert plot for a facetted layout, allowing for the comparison
#' of response distributions across multiple categories or groups within the dataset.
#'
#' @inheritParams create_base_likert_plot
#'
#' Additional parameters and customization options can be added as needed.
#'
#' @return A `ggplot2` object representing a facetted Likert plot.
#'
#' @noRd
create_facetted_likert <- function(data,
                                   group,
                                   addLabels,
                                   threshold,
                                   width,
                                   geom_size,
                                   logic

) {
  # Determine ncol
  ncol <- length(unique(data$Response))

  # Create facetted bar chart
  facetted_plot <- create_base_likert_plot(data, group, threshold, width, logic) +
    facet_wrap(.~Response, ncol = ncol)

  # Add labels
  if (addLabels) {
    facetted_plot <- facetted_plot +
      geom_text(aes(x = ifelse(round(Perc) >= 10, Perc + ncol, Perc + ncol-(ncol/4))),
                colour = colour_pal("Black80"),
                position = position_stack(vjust = 1, reverse = TRUE),
                size = convert_sizing(geom_size))
  }

  return(facetted_plot)
}

#' Adds NET Column to Likert Plot
#'
#' Augments a Likert plot with an additional column representing the NET score,
#' calculated as the difference between positive and negative responses. This function
#' also adjusts the plot based on additional aesthetics and logical parameters.
#'
#' @param data Processed data including NET scores.
#' @param width Width of the bars in the plot.
#' @param plot Initial ggplot object to which the NET column will be added.
#' @param group The grouping variable used to categorize data, affecting how NET scores are displayed.
#' @param geom_size Size parameter for the geometric objects in the plot, influencing the size of the NET column.
#' @param logic A character string indicating the logical condition for adjusting plot aesthetics. Possible values include "group_y" for adjusting based on the grouping variable and "group_facet" for facetting the plot according to the group.
#'
#' @return A ggplot object with an added NET column and adjusted according to specified logic and aesthetics.
#' @noRd
add_net_column <- function(data,
                           width,
                           plot,
                           group,
                           geom_size,
                           logic,
                           addLabels
) {
  if ("NET" %in% data$Id) {
    # font size
    size <- if (logic == "group_facet") convert_sizing(geom_size - 2.5) else convert_sizing(geom_size)

    # Subset NET data
    net_data <- data[data$Id == "NET",]

    # Determine the variable to use based on 'logic'
    y_aes <- if (logic == "group_y")net_data[[group]] else net_data$Question
    height <- if (logic == "group_y") width / 2 - 0.1 else width
    # TODO unsure why y = group vs y = Question affects the height

    # Get position of last factor
    net_label_y_pos <- max(as.numeric(factor(y_aes))) + height

    # Add column using tiles and text
    plot <- plot +
      geom_tile(data = net_data,
                aes(x = 105,
                    y = y_aes,
                    width = 5,
                    height = width),
                fill = colour_pal("Regent Grey"),
                alpha = 0.3) +
      geom_text(data = net_data,
                aes(x = 105,
                    y = y_aes,
                    label = ifelse(net_data$Perc >= 0,
                                   paste0("+", round(net_data$Perc)),
                                   as.character(round(net_data$Perc)))),
                colour = colour_pal("Black80"),
                size = size,
                hjust = 0.5) +

      # Add the "NET" label with a consistent nudge upwards
      geom_text(aes(x = 105,
                    y = net_label_y_pos,
                    label = "NET"),
                inherit.aes = FALSE, # Prevent inheriting aesthetics
                colour = colour_pal("Regent Grey"),
                size = size,
                nudge_y = if (!is.null(group)) 0.5 else 0,
                hjust = 0.5)

    if (addLabels == FALSE) {
      plot <- plot +
        # Manually set axis
        annotate("segment",
                 x = 0,
                 xend = 100,
                 y = 0.4,
                 yend = 0.4,
                 colour = colour_pal("French Grey"),
                 linewidth = 0.25)
    }

  }
  return(plot)
}
