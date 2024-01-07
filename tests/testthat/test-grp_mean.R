# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  expect_error(grp_mean("data"), "A data frame is required to be parsed through this function.")

  expect_error(grp_mean(df, var = "column1"), "`var` variable must be a column in `data`.")
  expect_error(grp_mean(df, var = "gender"), "`var` must be numeric.")

  expect_error(grp_mean(df, group = "column1"), "`group` variable must be a column in `data`.")

  expect_error(grp_mean(df, var = "gender", weight = "column1"), "`weight` variable must be a column in `data`.")
  expect_error(grp_mean(df, var = "gender", weight = "gender"), "`weight` must be numeric.")
})

# ==============================================================#
# TEST: PREPARE DATA

# ==============================================================#
# TEST: CALCULATE MEAN

# ==============================================================#
# TEST: SET COLUMN NAMES

# ==============================================================#
# TEST: RESULT
test_that("function returns grouped mean", {
  df2 <- data.frame(age = c(47,20,40,37,32,27,63,75),
                    gender = c("Male","Male","Male","Male","Male","Female","Female","Female"),
                    wt = c(1,0.9,0.6,0.22,1.5,1.1,1,0.99))
  # WITHOUT WEIGHTS
  # (class = Factor)
  # base r
  x <- grp_mean(df, var = "age", group = "gender",
                set_names = c("gender", "mean"), round_decimals = 2)

  # dplyr
  y <- df %>%
    group_by(gender) %>%
    summarise(mean = round(mean(age), 2), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[1, 2], y[1, 2])

  # (class = Character)
   # base r
  x <- grp_mean(df2, var = "age", group = "gender",
                set_names = c("gender", "mean"), round_decimals = 2)

  # dplyr
  y <- df2 %>%
    group_by(gender) %>%
    summarise(mean = round(mean(age), 2), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[1, 2], y[1, 2])

  # WITH WEIGHTS
  # (class = Factor)
  # base r
  x <- grp_mean(df, var = "age", group = "gender", weight = "wt")

  # dplyr
  y <- df %>%
    group_by(gender) %>%
    summarise(Mean = weighted.mean(age, wt), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[2, 2], y[2, 2])

  # (class = Character)
   # base r
  x <- grp_mean(df2, var = "age", group = "gender", weight = "wt")

  # dplyr
  y <- df2 %>%
    group_by(gender) %>%
    summarise(Mean = weighted.mean(age, wt), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[1, 2], y[1, 2])

})
