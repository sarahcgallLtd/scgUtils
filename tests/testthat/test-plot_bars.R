# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

test_that("errors returned correctly", {
  expect_error(plot_bars("data"), "Parameter `data` is required and must be a data frame.")
  expect_error(plot_bars(df, "invalid_column"), "`yVar` must be a column in `data`.")
  expect_error(plot_bars(df, "mii", group = "invalid_column"), "`group` must be a column in `data`.")
  expect_error(plot_bars(df, "mii", weight = "invalid_column"), "`weight` must be a column in `data`.")
  expect_error(plot_bars(df, "mii", weight = "gender"), "`weight` must be numeric.")
})

test_that("function returns plot correctly", {
  p <- plot_bars(df,
          "p_past_vote_2019",
          group = "gender",
          weight = "wt",
          round_decimals = 0)

  expect_equal(p$labels$fill, "p_past_vote_2019")
})
