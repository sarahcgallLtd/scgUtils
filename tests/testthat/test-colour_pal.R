# ==============================================================#
test_that("correct error message is thrown", {
  expect_error(colour_pal("invalid_palette"),
               "Palette not found.")

  expect_error(colour_pal("seqBlue", 8),
               "Number of requested colours is greater than what this palette can offer.")

  expect_warning(colour_pal("divRedBlue", n = 7, assign = c("hi1", "hi2", "hi3")),
               "Length of 'assign' is less than 'n'. Assignments will be repeated.")

  expect_warning(colour_pal("divRedBlue", n = 2, assign = c("hi1", "hi2", "hi3")),
                 "Length of 'assign' is greater than 'n'. Excess names will be ignored.")
})


# RETURN COLOURS
test_that("function returns colour palettes in the correct format", {
  p <- colour_pal("divRedBlue", n = 4, assign = c("hi1", "hi2", "hi3", "hi4"))
  expect_equal(class(p), "list")

  p <- colour_pal("polAus", assign = c("hi1", "hi2", "hi3", "hi4", "hi5", "hi6", "hi7"))
  expect_equal(class(p), "list")

  p <- colour_pal("polAus")
  expect_equal(class(p), "list")

  p <- colour_pal("catSimplified")
  expect_equal(class(p), "character")

  p <- colour_pal("catSimplified", type = "discrete_as")
  expect_equal(class(p), "list")

  p <- colour_pal("seqBlue", type = "continuous")
  expect_equal(class(p), "function")

})
