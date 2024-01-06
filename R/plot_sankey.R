#' @title Sankey Plot
#' @name plot_sankey
#'
#' @description Visualise the flow of data with a sankey.
#'
#' @param data A data frame containing flow data and percentages. This parameter is required.
#' @param source From variable (left side of sankey). This parameter is required.
#' @param target To variable (right side of sankey). This parameter is required.
#' @param value Percentage/Value flowing between source and target. This parameter is required.
#' @param units Unit of value to display in tooltip (requires single quotation marks around double
#'   quotations). Default = '""'
#' @param colours List of colours in d3.js format (requires single quotation marks around internal hex codes)
#' double quotation marks. This variable is required.
#' @param fontSize Default = 24
#' @param fontFamily Default = "Calabri"
#' @param nodeWidth Default = 20
#' @param nodePadding Default = 10
#' @param margin Default = list("left"=0, "right"=400)
#' @param width Default = 1200
#' @param height Default = 800
#' @param shiftLabel the default will automatically place labels to outside of the sankey, however if there
#'   are unequal numbers of labels, enter in a numeric value to control which labels shift from inside to out.
#'
#' @return sankey plot
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
#'             colours = '"#d62f24","#378cc5","#4fbe57","#fee043","#9b0d0c",
#'                        "#3e3e3f","#e69138ff","#b7b7b7ff","#cdcdd1","#cdcdd1"')
#'}
#' @export
plot_sankey <- function(data,
                        source,
                        target,
                        value,
                        units="",
                        colours,
                        fontSize=20,
                        fontFamily="Calibri",
                        nodeWidth=20,
                        nodePadding=10,
                        margin=list("left"=0, "right"=0),
                        width=1200,
                        height=800,
                        shiftLabel=NA
) {
  links <- data[,c(source,target,value)]
  links <- stats::setNames(links, c("source","target","value"))

  colnames(links) <- c("source", "target", "value")
  links$target <- paste(links$target, " ", sep="")
  nodes <- data.frame(name=c(as.character(links$source),
                             as.character(links$target)))
  nodes <- unique(nodes)
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1
  links$group <- as.factor(links$source)
  nodes$group <- as.factor(nodes$name)
  links <- as.data.frame(links)

  # ==============================================================#
  # d3 Preparations

  # Get colour scale
  colourScale <- paste0('d3.scaleOrdinal() .range([',colours, '])')

  # Get number of nodes to shift labels to the outside of the sankey
  if(is.na(shiftLabel)==TRUE){
    left <- as.character(nrow(nodes)/2)
  } else{
    # check parameter:
    if(!class(shiftLabel) %in% c("integer", "numeric")){
      stop("shiftLabel must be a numeric value only")
    }
    left <- shiftLabel
  }

  # Get total number of values column
  total <- sum(links$value)

  # Get units
 #test <-

  # ============================================================== #
  # Create Sankey
  sankey <- networkD3::sankeyNetwork(Links = links,
                                     Nodes = nodes,
                                     Source = "IDsource",
                                     Target = "IDtarget",
                                     Value = "value",
                                     LinkGroup="group",
                                     NodeID = "name",
                                     NodeGroup = "group",
                                     sinksRight=FALSE,
                                     colourScale=colourScale,
                                     fontSize=fontSize,
                                     fontFamily=fontFamily,
                                     nodeWidth=nodeWidth,
                                     nodePadding=nodePadding,
                                     margin=margin,
                                     width=width,
                                     height=height)

  htmlwidgets::onRender(
    sankey,
      paste0('
      function(el,x){

      // move source node text (Previous Action) to the left
      d3
      .select(el)
      .selectAll(".node text")
      .filter(function(d,i) { return i < ',left,'; })
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
      let total = d3.sum([',total,'])
      var percent = d3.format(".2%")
      var units = [',units,']

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