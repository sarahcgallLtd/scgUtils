#' @title Binary Survey Data
#' @name grid_vars
#'
#' @description Pivot binary grid survey questions longer.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param vars A list of column names and the renamed variable. This parameter is required and permits multiple variables.
#' @param group A variable overlay to compare between groups. This parameter is optional and only permits one group.
#' @param weight Variable containing weight factors. This variable is optional.
#'
#' @return a data frame of variables from a grid survey question.
#'
#' @examples
#' \dontrun{
#' # Create list
#' vars <- list(Q1a = "Art",
#'              Q1b = "Automobiles",
#'              Q1c = "Birdwatching")
#' grid_vars(dataset,
#'             vars = vars,
#'             group = "gender",
#'             weight = "wgtvar")
#'
#' #      Question Response gender     Freq Perc
#' #           Art      Yes   Male 275.3617   46
#' #   Automobiles      Yes   Male 320.1372   53
#' #  Birdwatching      Yes   Male 310.4357   52
#' #           Art      Yes Female 204.7525   48
#' #   Automobiles      Yes Female 212.0209   49
#' #  Birdwatching      Yes Female 203.9380   47
#' # ...
#'}
#' @export
grid_vars <- function(data,
                      vars,
                      group,
                      weight
) {
  # ==============================================================#
  # CHECK PARAMETERS
  check_params(data=data,
               vars=vars,
               group=group,
               weight=weight)

  if (missing(vars))
    stop("`vars` is required to be parsed through this function.")

  # ==============================================================#
  # PREPARE VARIABLES
  # X
  x <- dput(names(vars), file = nullfile()) # suppress output to console
  # Y
  y <- c(append_if_exists(as.list(match.call()[-1])[-c(1, 2)]), x)

  # ==============================================================#
  # TRANSFORM DATA
  # Subset data
  tmp <- data[, y]

  # Ensure all character classes are converted to factors
  tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)

  # Make data frame longer
  tmp <- tidyr::pivot_longer(tmp, cols = names(tmp[,x]), names_to = "Question", values_to = "Response")

  # Change names of vars to from column names to new names
  tmp$Question <-  unlist(vars)[tmp$Question]

  # Make Question variable factor
  tmp$Question <- factor(tmp$Question)

  # ==============================================================#
  # GET GROUPED FREQUENCY & PERCENTAGE
  # Return funciton arguments
  grp <- match.call(expand.dots = FALSE)

  # Substitute `grid_vars` for `grp_freq`
  grp[[1L]] <- quote(grp_freq)

  # Substitute `data` for `tmp`
  grp[["data"]] <- tmp

  # Substitute `groups` for vector
  grp[["groups"]] <- c("Question", "Response",
                      append_if_exists(as.list(grp[match("group", names(grp), 0)])))

  # Percent by groupsPercent
  grp[["groupsPercent"]] <- c("Question",
                         append_if_exists(as.list(grp[match("group", names(grp), 0)])))

  # Limit decimal places
  grp[["round_decimals"]] <- 2

  # Remove `vars` and `group` from arguments
  grp[["vars"]] <- NULL

  if (!missing(group))
    grp[["group"]] <- NULL

  # Evaluate
  tmp <- eval(grp, parent.frame())

  # ==============================================================#
  return(tmp)
}
