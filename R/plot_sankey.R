#' @title Sankey Diagram Visualisation
#' @name plot_sankey
#'
#' @description `plot_sankey` enhances
#' [`networkD3::sankeyNetwork`](https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/sankeyNetwork)
#' and [`htmlwidgets`](https://www.htmlwidgets.org/) to create a Sankey diagram, a specific type of flow diagram
#' that visually represents the movement or flow of data between different entities (referred to as "nodes").
#' It is particularly useful for illustrating the distribution of votes flowing between parties across elections
#' or in preferential voting systems.
#'
#' @param data A data frame containing the flow data, where each row represents a unique flow from a source to a target.
#' @param source The column in the data frame representing the source node of the flow.
#' @param target The column in the data frame representing the target node of the flow.
#' @param value The column in the data frame representing the magnitude or volume of the flow between the source and target.
#' @param units A string indicating the units of the 'value'. This is displayed in the tooltip of the diagram.
#'   The default is an empty string, indicating that no units are specified.
#' @param colours A list or vector of colours (in hexadecimal format or colour names) to be used for the nodes and links
#'   in the diagram. If a named list is provided, the names should correspond to the node names for specific
#'   colour mapping.
#' @param fontSize The font size for text in the diagram. Default is set to 20.
#' @param fontFamily The font family for text in the diagram. Default is "Calibri".
#' @param nodeWidth The width of the nodes (entities) in the diagram. Default is 20.
#' @param nodePadding The padding between nodes in the diagram. Default is 10.
#' @param margin A list specifying the margins around the diagram. Default is `list("left" = 0, "right" = 0)`.
#'   Increase the margin if the node names are long and go off the page.
#' @param width The width of the plot in pixels. Default is 1200.
#' @param height The height of the plot in pixels. Default is 800.
#' @param shiftLabel A numeric value controlling the positioning of labels on the nodes. When set to `NA`,
#'   labels are automatically placed outside the nodes. If the automatic placement results in uneven label
#'   distribution, this parameter can be adjusted to achieve the desired layout.
#'
#' @return A Sankey diagram visualising the flow of data as specified by the parameters. The diagram is interactive,
#'   allowing for the exploration of individual flows and nodes.
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' split <- grp_freq(df,
#'                   c("Q1a","Q1c"),
#'                   "wgtvar",
#'                   c("Party_Vote","Candidate_Vote","Votes")) %>%
#'          mutate(Votes = Votes/sum(Votes))
#' #           Party_Vote    Candidate_Vote        Votes
#' # 1       Labour Party      Labour Party 0.1036125880
#' # 2     National Party      Labour Party 0.1040036893
#' # 3        Green Party      Labour Party 0.0320547032
#' # ...
#'
#' # Create sankey
#' plot_sankey(data = split,
#'             source = "Party_Vote",
#'             target = "Candidate_Vote",
#'             value = "Votes",
#'             colours = colour_prep(df, c("Q1a", "Q1c"), pal_name = "polNZ"))
#'}
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
                        shiftLabel = NA
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data)

  requiredCols <- c(source, target, value)
  if (!all(requiredCols %in% names(data))) {
    stop("Source, target, and value columns must be present in the data.")
  }

  # ==============================================================#
  # PREPARE SANKEY DATA
  links <- data[, c(source, target, value)]
  links <- stats::setNames(links, c("source", "target", "value"))

  colnames(links) <- c("source", "target", "value")
  links$target <- paste(links$target, " ", sep = "")
  nodes <- data.frame(name = c(as.character(links$source),
                               as.character(links$target)))
  nodes <- unique(nodes)
  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1
  links$group <- as.factor(links$source)
  nodes$group <- as.factor(nodes$name)
  links <- as.data.frame(links)

  # ==============================================================#
  # PREPARE ATTRIBUTES
  # Prepare colours based on node/link names or order
  if (is.list(colours) && !is.data.frame(colours) || is.vector(colours) ) {
    # Convert color names to hex if necessary
    colours_hex <- sapply(colours, function(col) {
      if (!startsWith(col, "#")) {
        if (!col %in% colours()) {
          stop(paste("Invalid colour name:", col))
        }
        rgb_vals <- grDevices::col2rgb(col, alpha = FALSE) / 255
        return(grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 1))
      }
      return(col)
    }, USE.NAMES = TRUE)

    if (is.null(names(colours))) {
      # Unnamed list/vector, use colours in order
      colours_string <- paste(shQuote(colours_hex), collapse = ",")
    } else {
      # Extract unique groups from the source by converting to characters,
      # removing trailing white space and making unique
      unique_groups <- as.character(nodes$group)
      unique_groups <- unique(sapply(unique_groups, trimws))

      # Order the colours_hex by the order of unique_groups
      ordered_colours_hex <- colours_hex[unique_groups]

      # Named list/vector, match colours to node/link names
      colours_string <- paste(shQuote(unname(unlist(ordered_colours_hex))), collapse = ",")
    }
    colourScale <- paste0("d3.scaleOrdinal().range([", colours_string, "])")
  } else {
    stop("Colours must be a list or a vector.")
  }

  # Get number of nodes to shift labels to the outside of the sankey
  if (is.na(shiftLabel) == TRUE) {
    left <- as.character(nrow(nodes) / 2)
  } else {
    # check parameter:
    if (!class(shiftLabel) %in% c("integer", "numeric")) {
      stop("shiftLabel must be a numeric value only")
    }
    left <- shiftLabel
  }

  # Get total number of values column
  total <- sum(links$value)

  # ============================================================== #
  # CREATE SANKEY
  sankey <- networkD3::sankeyNetwork(Links = links,
                                     Nodes = nodes,
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