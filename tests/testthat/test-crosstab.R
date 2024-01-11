# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  expect_error(crosstab("data"),
               "A data frame is required to be parsed through this function.")
  expect_error(crosstab(),
               "A data frame is required to be parsed through this function.")

  # colVar and rowVar ==============================================#
  expect_error(crosstab(df, rowVar = "column1"),
               "`rowVar` variable must be a column in `data`.")
  expect_error(crosstab(df, colVar = "column1"),
               "`colVar` variable must be a column in `data`.")
  expect_error(crosstab(df),
               "`rowVar` and `colVar` are required to be parsed through this function.")

  # weight =====================================================#
  expect_error(crosstab(df, weight = "column1"),
               "`weight` variable must be a column in `data`.")
  expect_error(crosstab(df, weight = "gender"),
               "`weight` must be numeric.")
})

# ==============================================================#
# TEST: DEPARSE
# test_that("returns quoted argument only", {
#   expect_equal(test_deparse("gender"), "gender")
#   expect_equal(test_deparse(gender), "gender")
# })

# ==============================================================#
# TEST: CALCULATE XTAB
test_that("returns xtab_calc correctly", {
  vars <- c("gender","ageGroup")

  # unweighted
  expect_equal(xtab_calc(df, vars)[14,3], 1215)

  # weighted
  expect_equal(xtab_calc(df, vars, "wt")[14,3], 767.50818)

  # include statistics
  expect_equal(as.character(xtab_calc(df, vars, type="statistics")),
               "gender x ageGroup: Chisq = 28.441 | DF = 5 | Cramer's V = 0.075 | p-value = 0")

  # include statistics_df
  expect_equal(xtab_calc(df, vars, type="statistics_df")$Chisq,28.441)

})

# TEST: CALCULATE XTAB TOTAL
test_that("returns xtab_total correctly", {
  vars <- c("gender","ageGroup")
  data <- xtab_calc(df, vars, weight = "wt")

  expect_length(xtab_totals(data, "gender","ageGroup"), 3)
  expect_length(xtab_totals(data, "gender","ageGroup", "name"), 3)

})

# TEST: CALCULATE XTAB TOTAL
test_that("returns xtab_convert correctly", {
  vars <- c("gender","ageGroup")
  data <- xtab_calc(df, vars, weight = "wt")

  expect_length(xtab_totals(data, "gender","ageGroup"), 3)
  expect_length(xtab_totals(data, "gender","ageGroup", "name"), 3)

})

# TEST PIVOT
test_that("returns pivot_wide", {
  vars <- c("gender","ageGroup")
  data <- xtab_calc(df, vars, weight = "wt")

  x <- pivot_wide(data,vars)
  y <- as.data.frame(tidyr::pivot_wider(data, names_from = "ageGroup", values_from = Freq))

  expect_equal(x, y)

})

# ==============================================================#
# TEST: FORMATTING OPTIONS
test_that("function returns correct format", {
  # basic defaults
  expect_length(crosstab(df, "gender","ageGroup"), 4)
  # with everything turned off
  expect_length(crosstab(df, "gender","ageGroup", "wt",
                         FALSE,2,FALSE,FALSE), 4)
  # with weight and long ways
  expect_length(crosstab(df, "gender","ageGroup", "wt",
                        FALSE,2,FALSE,FALSE, format="df_wide"), 8)
  # with weight and csv
  expect_length(crosstab(df, "gender","ageGroup", "wt",
                        FALSE,2,FALSE,FALSE, format="csv"), 8)
  # with weight and statistics
  expect_length(crosstab(df, "gender","ageGroup", "wt",
                        FALSE,2,FALSE,FALSE, format="statistics"), 7)
  # with weight and frequency
  expect_length(crosstab(df, "gender","ageGroup", "wt",
                         FALSE,2,FALSE,FALSE, convert_to="frequency"), 3)

})
