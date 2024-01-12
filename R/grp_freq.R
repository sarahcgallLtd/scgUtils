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
#'
#' #    age_categories gender Freq
#' # 1        18 to 24   Male   84
#' # 2        25 to 34   Male  155
#' # 3        35 to 44   Male  180
#' # 4        45 to 54   Male  115
#' # 5        55 to 54   Male   38
#' # 6             65+   Male    6
#' # 7        18 to 24 Female   83
#' # 8        25 to 34 Female  153
#' # 9        35 to 44 Female  129
#' # 10       45 to 54 Female   57
#' # 11       55 to 54 Female   24
#' # 12            65+ Female    6
#'
#' # Sum weighted data by group and change the column names
#' df <- grp_freq(dataset,
#'                group = c("age_categories","gender"),
#'                weight = "wgtvar",
#'                set_names = c("Age","Gender","n"))
#'
#' #         Age Gender         n
#' # 1  18 to 24   Male  43.40468
#' # 2  25 to 34   Male 115.36419
#' # 3  35 to 44   Male 160.55258
#' # 4  45 to 54   Male 187.29274
#' # 5  55 to 54   Male  61.88803
#' # 6       65+   Male  30.90000
#' # 7  18 to 24 Female  42.88796
#' # 8  25 to 34 Female 109.82791
#' # 9  35 to 44 Female 115.06268
#' # 10 45 to 54 Female  92.83205
#' # 11 55 to 54 Female  39.08718
#' # 12      65+ Female  30.90000
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

  # `groups` is required
  if (missing(groups))
    stop("`groups` is required to be parsed through this function.")

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
