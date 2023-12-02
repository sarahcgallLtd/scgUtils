#' @title Grouped Frequencies
#' @name grp_freq
#'
#' @description Calculates the frequency of a variable by groups in survey data.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param group Vector of variables being grouped. This parameter is required.
#' @param weight Variable containing weight factors. This variable is optional.
#' @param set_names Vector of column names. This paramenter is optional.
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
grp_freq <- function(data, group, weight, set_names) {
   grp <- list()
   for (x in group) {
      y <- data[,x]
      grp[[x]] <- y
   }

  if (missing(weight)) {
    df <- stats::aggregate(grp[[1]], by=unlist(grp,recursive=FALSE), FUN=length)
  } else {
    df <- stats::aggregate(data[,weight], by=unlist(grp,recursive=FALSE), FUN=sum)
  }

  if (!missing(set_names)) {
    df <- stats::setNames(df, set_names)
  } else {
    df <- stats::setNames(df, c(group,"Freq"))
  }

  return(df)
}
