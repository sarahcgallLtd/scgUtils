#' @title Grouped Frequencies
#' @name grp_freq
#'
#' @description Calculates the frequency of a variable by groups in survey data.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param groups Vector of variables being grouped. This parameter is required and permits one or many variables.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param set_names Vector of column names. This paramenter is optional.
#' @param addPercent Get percent of frequency (Options: "no" or "yes"). This parameter is optional, defaulted to "No", and permits one or many variables.
#' @param groupsPercent Vector of variables for percent of frequency. This parameter is optional and permits none (for total), one or many variables.
#' @param round_decimals Numeric value to round numeric data by x number of decimals places. Default does not round. This parameter is optional.
#'
#' @return A data frame containing frequencies by group
#'
#' @examples
#' \dontrun{
#' # Sum unweighted data by group
#' df <- grp_freq(dataset,
#'                group = c("age_categories","gender"))
#' # Sum weighted data by group and change the column names
#' df <- grp_freq(dataset,
#'                group = c("age_categories","gender"),
#'                weight = "wgtvar",
#'                set_names = c("Age","Gender","n"))
#' }
#' @export
grp_freq <- function(data,
                     groups,
                     weight = NULL,
                     set_names = NULL,
                     addPercent = c("no", "yes"),
                     groupsPercent = NULL,
                     round_decimals = NULL
) {
  # ==============================================================#
  # CHECK PARAMS
  check_params(data = data,
               groups = groups,
               weight = weight,
               groupsPercent = groupsPercent)

  # Take first option
  addPercent <- match.arg(addPercent)

  # ==============================================================#
  # PREPARE DATA
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # make "group_by" a list
  grp <- list_group(data, groups)

  # ==============================================================#
  # GET FREQUENCY
  tmp <- if (is.null(weight)) {
    stats::aggregate(grp[[1]], by = grp, FUN = length)
  } else {
    stats::aggregate(data[, weight], by = grp, FUN = sum)
  }

  # ==============================================================#
  # SET COLUMN NAMES
  if (!is.null(set_names)) {
    tmp <- stats::setNames(tmp, set_names)
  } else {
    tmp <- stats::setNames(tmp, c(groups, "Freq"))
  }

  # ==============================================================#
  # OPTIONAL: ADD PERCENT
  if (addPercent == "yes" || !is.null(groupsPercent)) {
    if (missing(groupsPercent))
      tmp <- transform(tmp, Perc = stats::ave(Freq, FUN = function(x) x / sum(x) * 100))
    else
      tmp <- transform(tmp, Perc = stats::ave(Freq, tmp[, groupsPercent],
                                            FUN = function(x) x / sum(x) * 100))

    if (is.numeric(round_decimals) == TRUE)
      tmp <- round_vars(tmp, round_decimals)
  }

  # ==============================================================#
  return(tmp)
}
