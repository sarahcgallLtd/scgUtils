#' @title Visualise Data Flow with a Sankey Diagram
#' @name plot_sankey
#'
#' @description
#' `plot_sankey` enhances the [`networkD3::sankeyNetwork`](https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/sankeyNetwork)
#' function, utilising [`htmlwidgets`](https://www.htmlwidgets.org/) to create an interactive Sankey diagram,
#' which is a specific type of flow diagram. It's especially effective for illustrating
#' data movement or transfer between different entities (nodes), such as the distribution
#' of votes flowing between parties across elections or in preferential voting systems.
#'
#' @param data A data frame containing the flow data, where each row represents a unique flow
#'   from a source to a target node.
#' @param source The column in `data` representing the source nodes.
#' @param target The column in `data` representing the target nodes.
#' @param value The column in `data` representing the flow's magnitude or volume.
#' @param units A string indicating the units for `value`, displayed in the tooltip.
#'   Defaults to an empty string, implying no units.
#' @param colours A list or vector of colours for nodes and links in the diagram, either
#'   as hexadecimal codes or colour names. Can be a named list for specific node colour mapping.
#' @param fontSize Font size for text in the diagram.
#' @param fontFamily Font family for text in the diagram.
#' @param nodeWidth Width of nodes in the diagram.
#' @param nodePadding Padding between nodes in the diagram.
#' @param margin Margins around the diagram, specified as a list.
#' @param width Width of the plot in pixels.
#' @param height Height of the plot in pixels.
#' @param shiftLabel Numeric value to adjust the position of labels; `NA` for auto-placement.
#'
#' @return An interactive Sankey diagram as an HTML widget, which can be used in R Markdown documents,
#'   Shiny applications, or the R console.
#'
#' @examples
#' \dontrun{
#'   # Example: Visualizing the flow of votes between parties
#'   plot_sankey(data = election_data,
#'               source = "Party_Previous_Election",
#'               target = "Party_Current_Election",
#'               value = "Number_of_Votes",
#'               colours = c("Labour" = "red", "Green" = "green", "National" = "blue"))
#' }
#'
#' @export
plot_sankey <- function(data,
                        source,
                        target,
                        value,
                        units = "",
                        colours = colour_pal("catExtended"),
                        fontSize = 20,
                        fontFamily = "Calibri",
                        nodeWidth = 20,
                        nodePadding = 10,
                        margin = list("left" = 0, "right" = 0),
                        width = 1200,
                        height = 800,
                        shiftLabel = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data)

  requiredCols <- c(source, target, value)
  if (!all(requiredCols %in% names(data))) {
    stop("Source, target, and value columns must be present in the data.")
  }

  # ==============================================================#
  # Prepare Sankey data
  sankey_data <- prepare_sankey_data(data, source, target, value)

  # Prepare color scale
  colourScale <- prepare_colour_scale(colours, sankey_data$nodes)

  # Calculate left shift for labels
  left <- calculate_left_shift(sankey_data$nodes, shiftLabel)

  # Get total number of values column
  total <- sum(sankey_data$links$value)

  # ============================================================== #
  # CREATE SANKEY
  sankey <- networkD3::sankeyNetwork(Links = sankey_data$links,
                                     Nodes = sankey_data$nodes,
                                     Source = "IDsource",
                                     Target = "IDtarget",
                                     Value = "value",
                                     LinkGroup = "group",
                                     NodeID = "name",
                                     NodeGroup = "group",
                                     sinksRight = FALSE,
                                     colourScale = colourScale,
                                     fontSize = fontSize,
                                     fontFamily = fontFamily,
                                     nodeWidth = nodeWidth,
                                     nodePadding = nodePadding,
                                     margin = margin,
                                     width = width,
                                     height = height)

  htmlwidgets::onRender(
    sankey,
    paste0('
      function(el,x){

      // move source node text (Previous Action) to the left
      d3
      .select(el)
      .selectAll(".node text")
      .filter(function(d,i) { return i < ', left, '; })
      .attr("x", x.options.nodeWidth - 30)
      .attr("text-anchor", "end");

      // change outline colour of boxes
      d3
      .selectAll("rect")
      .filter(function(d,i) { return i >= 0; })
      .attr("stroke-opacity", 0)
      .attr("fill-opacity", 1);

      // change opacity of links
      d3
      .selectAll(".link")
      .style("opacity",0.5);

      // change text to bold
      d3
      .selectAll(".node text")
      .style("font-weight", 400);

      // change text to bold
      d3
      .selectAll(".node text")
      .style("fill", function(d) { return(d) });

      // change node tooltip
      let total = d3.sum([', total, '])
      var percent = d3.format(".2%")
      var units = [', units, ']

      d3
      .selectAll(".node")
      .select("title foreignObject body pre")
      .text(function(d) { return d.name + ": " + "\\n" +
       d3.format(",.0f")(d.value) + units + " (" +
       percent(d.value/total) +
       ")"; });

       // change links tooltip
      d3
      .selectAll(".link")
      .select("title foreignObject body pre")
      .text(function(d) { return d.source.name + " \u2192 " + d.target.name + "\\n" +
      d3.format(",.0f")(d.value) + units; });

      // have control over link colour
      }')
  )
}

#' Prepare Data for Sankey Diagram
#'
#' This internal helper function for `plot_sankey` prepares the link and node data
#' required for creating a Sankey diagram.
#'
#' @param data A data frame containing the raw data for the Sankey diagram.
#' @param source The column in `data` representing the source nodes.
#' @param target The column in `data` representing the target nodes.
#' @param value The column in `data` representing the flow values between nodes.
#'
#' @return A list containing two data frames: `links` and `nodes`, formatted for use in a Sankey diagram.
#'
#' @details
#' The function creates a `links` data frame with source, target, and value columns,
#' along with ID mappings for source and target nodes. It also generates a `nodes`
#' data frame with unique node names and their respective groups.
#'
#' @noRd
prepare_sankey_data <- function(data,
                                source,
                                target,
                                value
) {
  links <- data[, c(source, target, value)]
  links <- stats::setNames(links, c("source", "target", "value"))

  #colnames(links) <- c("source", "target", "value")
  links$target <- paste(links$target, " ", sep = "")
  nodes <- unique(data.frame(name = c(as.character(links$source),
                                      as.character(links$target))))
  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1
  links$group <- as.factor(links$source)
  nodes$group <- as.factor(nodes$name)
  #links <- as.data.frame(links)
  return(list(links = links, nodes = nodes))
}

#' Prepare Colour Scale for Sankey Diagram
#'
#' An internal helper function for `plot_sankey` that prepares a colour scale
#' for the nodes and links of the Sankey diagram.
#'
#' @param colours A list or vector of colours, either as names or hexadecimal codes.
#' @param nodes A data frame of nodes prepared by `prepare_sankey_data`.
#'
#' @return A string representing the D3 colour scale function for the Sankey diagram.
#'
#' @details
#' The function converts colour names to hexadecimal codes if necessary, and then
#' constructs a colour scale string in D3 format. If the colours are unnamed, they are
#' used in the order provided; if named, they are matched to node names.
#'
#' @noRd
prepare_colour_scale <- function(colours, nodes) {
  # Convert colour names to hex if necessary
  colours_hex <- convert_colours(colours)

  if (is.null(names(colours))) {
    # Unnamed list/vector, use colours in order
    colours_string <- paste(shQuote(colours_hex), collapse = ",")
  } else {
    # Extract unique groups from the source by converting to characters,
    # removing trailing white space and making unique
    unique_groups <- unique(sapply(as.character(nodes$group), trimws))

    # Order the colours_hex by the order of unique_groups
    ordered_colours_hex <- colours_hex[unique_groups]

    # Named list/vector, match colours to node/link names
    colours_string <- paste(shQuote(unname(unlist(ordered_colours_hex))), collapse = ",")
  }
  colourScale <- paste0("d3.scaleOrdinal().range([", colours_string, "])")

  return(colourScale)
}

#' Calculate Shift for Node Labels in Sankey Diagram
#'
#' This internal helper function for `plot_sankey` calculates the amount of shift
#' needed for node labels on the left side of the Sankey diagram.
#'
#' @param nodes A data frame of nodes prepared by `prepare_sankey_data`.
#' @param shiftLabel A numeric value indicating the amount of shift or `NULL` to auto-calculate.
#'
#' @return A numeric value representing the shift for node labels.
#'
#' @details
#' If `shiftLabel` is provided, it is used as the shift amount. Otherwise, the function
#' automatically calculates the shift based on the number of nodes. The shift is used
#' to adjust the position of node labels in the Sankey diagram.
#'
#' @noRd
calculate_left_shift <- function(nodes,
                                 shiftLabel
) {
  # Get number of nodes to shift labels to the outside of the sankey
  if (is.null(shiftLabel)) {
    return(as.character(nrow(nodes) / 2))
  } else {
    # check parameter:
    if (!class(shiftLabel) %in% c("integer", "numeric")) stop("shiftLabel must be a numeric value only")
    return(shiftLabel)
  }
}