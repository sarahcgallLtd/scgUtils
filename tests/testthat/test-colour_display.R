test_that("correct error message is returned", {
  expect_error(colour_display("polUK",type="continuous"),
               "continuous type can only used with sequential or divergent palettes")
})

test_that("function returns correctly", {
  p <- colour_display("polUK")
  expect_equal(p$labels$title, "polUK (n= 12)")

  p <- colour_display("Jaffa")
  expect_equal(p$labels$title, "Jaffa (n= 1)")

  p <- colour_display("All")
  expect_equal(p$labels$title, "All (n= 28)")
  expect_identical(p$facet$params$as.table, TRUE)

  p <- colour_display("seqGreen", 7)
  expect_equal(p$labels$title, "seqGreen (n= 7)")

  p <- colour_display("divBlueGreen", 5, c("Very Likely", "Likely", "Neutral", "Unlikely", "Very Unlikely"))
  expect_equal(p$labels$title, "divBlueGreen (n= 5)")
  expect_equal(p$data$value, c("Very Likely", "Likely", "Neutral", "Unlikely", "Very Unlikely"))

  p <- colour_display("catSimplified")
  expect_equal(p$labels$title, "catSimplified (n= 7)")

  p <- colour_display("divRedBlue", type="continuous")
  expect_equal(p$guides$guides$fill$params$name, "colourbar")
})
