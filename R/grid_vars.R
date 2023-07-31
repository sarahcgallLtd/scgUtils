#' @title Binary Survey Data
#' @name grid_vars
#'
#' @description Pivot binary grid survey questions longer.
#'
#' @param data A data frame containing survey data. This parameter is required.
#' @param vars A list of column names and the renamed variable. This parameter is required.
#' @param group A variable overlay to compare between groups. This parameter is optional.
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
  # vars
  x <- dput(names(vars))

  if (!missing(weight) && !missing(group)) {
    y <- c(c(group, weight),x)
  } else if (!missing(weight) && missing(group)) {
    y <- c(c(weight),x)
  } else if (missing(weight) && !missing(group)) {
    y <- c(c(group),x)
  } else if (missing(weight) && missing(group)) {
    y <- c(x)
  }

  df <- data[,y]
  df <- tidyr::pivot_longer(df, cols = x, names_to = "Question", values_to = "Response")
  df <- dplyr::mutate(df, Question = dplyr::coalesce(unlist(vars)[Question], Response))
  df$Question <- factor(df$Question)

  # group by Question, Response, Group (Frequency)
  grp <- match.call(expand.dots = FALSE)
  grp_n <- match(c("weight"), names(grp), 0L)
  grp <- grp[c(1L, grp_n)]
  grp[[1L]] <- quote(grp_freq)
  grp[["data"]] <- as.data.frame(df)
  if (!missing(group))
    grp[["group"]] <- c("Question","Response",group)
  else
    grp[["group"]] <- c("Question","Response")

  df <- eval(grp, parent.frame())

  # Group by Group, Question (Percent)
  if (!missing(group))
    df <- transform(df, Perc = stats::ave(Freq, Question, df[,group],
                                      FUN = function(x) round(x/sum(x)*100, 2)))
  else
    df <- transform(df, Perc = stats::ave(Freq, Question,
                                      FUN = function(x) round(x/sum(x)*100, 2)))

  return(df)
}
