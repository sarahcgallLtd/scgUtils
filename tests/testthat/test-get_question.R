test_that("function returns question label", {
  df <- get_data("survey")
  df <- labelled::unlabelled(df)

  expect_equal(get_question(df,"p_eurefvote"), "EU referendum vote (earliest recorded)")
})
