test_that("function returns survey dataset", {
  df <- get_data("survey")

  expect_length(df, 65)
})