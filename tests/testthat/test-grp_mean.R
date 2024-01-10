# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  expect_error(grp_mean("data"),
               "A data frame is required to be parsed through this function.")
  expect_error(grp_mean(),
               "A data frame is required to be parsed through this function.")

  # var ========================================================#
  expect_error(grp_mean(df, meanVar = "column1"),
               "`meanVar` variable must be a column in `data`.")
  expect_error(grp_mean(df, meanVar = "gender"),
               "`meanVar` must be numeric.")
  expect_error(grp_mean(df),
               "`meanVar` is required to be parsed through this function.")

  # group ======================================================#
  expect_error(grp_mean(df, groups = "column1"),
               "`groups` variable must be columns in `data`.")
  expect_error(grp_mean(df, meanVar = "age"),
               "`groups` is required to be parsed through this function.")

  # weight =====================================================#
  expect_error(grp_mean(df, meanVar = "gender", weight = "column1"),
               "`weight` variable must be a column in `data`.")
  expect_error(grp_mean(df, meanVar = "gender", weight = "gender"),
               "`weight` must be numeric.")
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
                    gender2 = c("Other","Male","Male","Male","Male","Female","Female","Female"),
                    wt = c(1,0.9,0.6,0.22,1.5,1.1,1,0.99))
  # WITHOUT WEIGHTS
  # (class = Factor)
  # base r
  x <- grp_mean(df, meanVar = "age", groups = "gender",
                set_names = c("gender", "mean"), round_decimals = 2)

  # dplyr
  y <- df %>%
    group_by(gender) %>%
    summarise(mean = round(mean(age), 2), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[1, 2], y[1, 2])

  x <- grp_mean(df2, meanVar = "age", groups = c("gender","gender2"))

  # dplyr
  y <- df2 %>%
    group_by(gender, gender2) %>%
    summarise(mean = mean(age), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[3, 3], y[3, 3])

  # (class = Character)
   # base r
  x <- grp_mean(df2, meanVar = "age", groups = "gender",
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
  x <- grp_mean(df, meanVar = "age", groups = "gender", weight = "wt")

  # dplyr
  y <- df %>%
    group_by(gender) %>%
    summarise(Mean = weighted.mean(age, wt), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[2, 2], y[2, 2])

  # (class = Character)
   # base r
  x <- grp_mean(df2, meanVar = "age", groups = "gender", weight = "wt")

  # dplyr
  y <- df2 %>%
    group_by(gender) %>%
    summarise(Mean = weighted.mean(age, wt), .groups = "drop") %>%
    as.data.frame()

  expect_equal(x[1, 2], y[1, 2])

})
