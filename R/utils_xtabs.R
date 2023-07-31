# HELPER FUNCTIONS FOR CROSSTAB AND COMPILE FUNCTIONS

# Function to create crosstab and retrive statistics
xtab_calc <- function(data, formula, weight, statistics, type="xtabs_df") {
  df <- stats::xtabs(
        stats::reformulate(
          formula,
          response=weight
        ), data=data
      )
  x <- summary(df)
  k <- min(length(df),nrow(df))
  n <- x[[2]]
  chisq <- x[[3]]
  cramersv <- sqrt(chisq / (n * (k - 1)))

  p <- round(x[[6]],3)

  stat <- paste0(formula[1], " x ",formula[2], ": ",
                 "Chisq = ", round(chisq,3), " | ",
                 "DF = ", x[[4]], " | ",
                 "Cramer's V = ", round(cramersv,3), " | ",
                 "p-value = ", p
                 )

  if (type=="xtabs_df"){
    df <- as.data.frame(df)
    if (statistics==TRUE)
      print(noquote(stat))
  } else if (type=="statistics") {
    df <- data.frame(stat)
    names(df)[names(df) == "stat"] <- formula[1]
  } else if (type=="statistics_df") {
    df <- data.frame(Row_Var = formula[1],
                     Col_Var = formula[2],
                     Size = n,
                     Chisq = round(chisq,3),
                     DF = x[[4]],
                     CramersV = round(cramersv,3),
                     p_value = p
                    )
  }
  return(df)
}

# Function to add totals and sample size to df
xtab_totals <- function(data, row_var, col_var, name) {
  if (missing(name)){
    # Create column-wise totals
    total <- stats::aggregate(data$Freq, by=list(y=c(data[,1])), FUN=sum)
    total$z <- "Total"
    names(total)[names(total) == "x"] <- "Freq"
    names(total)[names(total) == "y"] <- {{ row_var }}
    names(total)[names(total) == "z"] <- {{ col_var }}
    df <- dplyr::bind_rows(data, total)
  } else {
    # Create row-wise totals
    df <- janitor::adorn_totals(
      data,
      where = "row",
      name = name
    )
  }
}

# Function to convert frequencies to percent (more options to be added in future)
xtabs_convert <- function(data, convert_to, format) {
  if (convert_to=="percent"){
    if (format=="csv"){
      formula = stats::as.formula(~ ./sum(.))
    } else
      formula = stats::as.formula(~ ./sum(.)*100)

    if (format==("df_long")) {
      df <- transform(data, `Perc` = stats::ave(Freq, data[,2], FUN = prop.table))
      df <- df %>%
        dplyr::mutate(Perc = as.numeric(df$Perc * 100))
    } else
      df <- data %>%
        dplyr::mutate(
          dplyr::across(
                      .cols=dplyr::where(is.numeric),
                      .fns=formula
          )
        )
    }
}
