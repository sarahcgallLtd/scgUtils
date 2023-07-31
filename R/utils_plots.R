# HELPER FUNCTIONS FOR PLOTS

# Function to determine brightness of colour for contrast against text colour
contrast_test <- function(colour_list) {
  col.list <- data.frame()
  for (x in unname(unlist(colour_list))) {
    contrast <- (sum( grDevices::col2rgb(x) * c(299, 587,114))/1000 < 123)
    col.list <- rbind(col.list, contrast)
  }
  colnames(col.list)[1] <- "contrast"

  return(col.list)
}

# Function to return minimum or maximum value based on numeric or factor
get_lims <- function(data, var, type=c("min","max")) {
  if (class(data[,var])=="numeric") {
    if (type=="min")
      x <- min(data[,var])
    else if (type=="max")
      x <- max(data[,var])
  } else if (class(data[,var])=="factor") {
    if (type=="min")
      x <- which.min(data[,var])
    else if (type=="max")
      x <- which.max(data[,var])
  }
  return(x)
}

# Function to create radar charts
coord_radar <- function(theta = "x", start = 0, direction = 1) {
        theta <- match.arg(theta, c("x", "y"))
        ggproto("CoordRadar", CoordPolar,
                theta = theta,
                r = ifelse(theta=='x','y','x'),
                start = start,
                direction = sign(direction),
                is_linear = function(coord) TRUE,
                clip="off")
}
