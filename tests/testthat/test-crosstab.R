# Data
# (factors)
df <- get_data("survey")
df <- labelled::unlabelled(df)
#sjPlot::view_df(df, show.frq = T, show.prc = T, show.na = T)
# (characters)
# file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
# df1 <- get_file(file_path)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  expect_error(crosstab("data"),
               "Parameter `data` is required and must be a data frame.")

  expect_error(crosstab(df, rowVar = "gender", colVar = "gender"),
               "`rowVar` and `colVar` must be two different variables.")

  expect_error(crosstab(df, rowVar = "gender", colVar = "ageGroup", weight = "column1"),
               "`weight` must be a column in `data`.")

  expect_error(crosstab(df, rowVar = "gender", colVar = "ageGroup", weight = "gender"),
               "`weight` must be numeric.")
})

# ==============================================================#
# TEST: CALCULATE XTAB
test_that("returns xtab_calc correctly", {
  vars <- c("gender", "ageGroup")

  # unweighted
  expect_equal(xtab_calc(df, vars, weight = NULL, statistics = FALSE)[14, 3], 1215)
  # expect_equal(xtab_calc(df1, vars, weight = NULL, statistics = FALSE)[11, 3], 1215)

  # weighted
  expect_equal(xtab_calc(df, vars, "wt", FALSE)[14, 3], 767.50818)
  # expect_equal(xtab_calc(df1, vars, "wt", FALSE)[11, 3], 767.50818)

  # include statistics
  expect_equal(as.character(xtab_calc(df, vars, NULL, FALSE, type = "statistics")),
               "gender x ageGroup: Chisq = 28.441 | DF = 5 | Cramer's V = 0.075 | p-value = 0")
  # expect_equal(as.character(xtab_calc(df1, vars, NULL, FALSE, type = "statistics")),
  #              "gender x ageGroup: Chisq = 28.441 | DF = 5 | Cramer's V = 0.075 | p-value = 0")

  # include statistics_df
  expect_equal(xtab_calc(df, vars, NULL, FALSE, type = "statistics_df")$Chisq, 28.441)
  # expect_equal(xtab_calc(df1, vars, NULL, FALSE, type = "statistics_df")$Chisq, 28.441)
})

# TEST: CALCULATE XTAB TOTAL
test_that("returns xtab_total correctly", {
  vars <- c("gender", "ageGroup")
  data <- xtab_calc(df, vars, "wt", FALSE)
  # data1 <- xtab_calc(df1, vars, "wt", FALSE)

  expect_length(xtab_totals(data, "gender", "ageGroup"), 3)
  # expect_length(xtab_totals(data1, "gender", "ageGroup"), 3)
  expect_length(xtab_totals(data, "gender", "ageGroup", "name"), 3)
  # expect_length(xtab_totals(data1, "gender", "ageGroup", "name"), 3)

})

# TEST: CALCULATE XTAB TOTAL
test_that("returns xtab_totals correctly", {
  vars <- c("gender", "ageGroup")
  data <- xtab_calc(df, vars, "wt", FALSE)
  # data1 <- xtab_calc(df1, vars, "wt", FALSE)

  x <- xtabs_convert(data, "percent", "df_long")
  expect_equal(x[14, 4], 48.55974)

  y <- pivot_wide(data, vars)
  y <- xtabs_convert(y, "percent", "df_wide")
  expect_equal(xtabs_convert(y, "percent", "df_wide")[2, 8], x[14, 4])

  # x <- xtabs_convert(data1, "percent", "df_long")
  # expect_equal(x[11, 4], 48.55974)

  # y <- pivot_wide(data1, vars)
  # y <- xtabs_convert(y, "percent", "df_wide")
  # expect_equal(xtabs_convert(y, "percent", "df_wide")[1, 7], x[11, 4])

})


# TEST PIVOT
test_that("returns pivot_wide", {
  vars <- c("gender", "ageGroup")
  data <- xtab_calc(df, vars, weight = "wt", FALSE)
  # data1 <- xtab_calc(df1, vars, weight = "wt", FALSE)

  x <- pivot_wide(data, vars)
  y <- as.data.frame(tidyr::pivot_wider(data, names_from = "ageGroup", values_from = Freq))

  expect_equal(x, y)

  # x <- pivot_wide(data1, vars)
  # y <- as.data.frame(tidyr::pivot_wider(data1, names_from = "ageGroup", values_from = Freq))
  #
  # expect_equal(x, y)

})

# ==============================================================#
# TEST: FORMATTING OPTIONS
test_that("function returns correct format", {
  # basic defaults
  expect_length(crosstab(df, "gender", "ageGroup"), 4)
  # expect_length(crosstab(df1, "gender", "ageGroup"), 4)
  # with everything turned off
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, TRUE), 4)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, TRUE), 4)
  # with weight and long ways
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, format = "df_wide"), 8)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, format = "df_wide"), 7)
  # with weight and csv
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, format = "csv"), 8)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, format = "csv"), 7)
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, format = "csv"), 8)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, format = "csv"), 7)
  # with weight and statistics
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, format = "statistics"), 7)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, format = "statistics"), 7)
  # with weight and frequency
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, convert_to = "frequency"), 3)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, convert_to = "frequency"), 3)
  # wide with plot
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         FALSE, 2, plot = TRUE, format = "df_wide"), 8)
  # expect_length(crosstab(df1, "gender", "ageGroup", "wt",
  #                        FALSE, 2, format = "df_wide"), 7)
  # wide with plot and totals
  expect_length(crosstab(df, "gender", "ageGroup", "wt",
                         TRUE, 2, plot = TRUE, format = "csv",
                         adjustX = TRUE), 9)

})
