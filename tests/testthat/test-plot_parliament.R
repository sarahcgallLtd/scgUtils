uk_parliament <- data.frame(
  Party = c("Labour", "SNP", "Other", "Liberal Democrat", "Conservative"),
  Seats = c(202, 48, 24, 11, 365),
  Percentage = c(32.1, 3.9, 8.8, 11.6, 43.6)
)

test_that("function returns correct errors", {
  expect_error(plot_parliament("data"),
               "Parameter `data` is required and must be a data frame.")
  expect_error(plot_parliament(uk_parliament, "invalid_column", "Seats"),
               "`partyCol` must be a column in `data`.")
  expect_error(plot_parliament(uk_parliament, "Party", "invalid_column"),
               "`seatCol` must be a column in `data`.")
  expect_error(plot_parliament(uk_parliament, "Party", "Party"),
               "`seatCol` must be numeric.")
  expect_error(plot_parliament(uk_parliament, "Party", "Seats", "Party"),
               "`percentCol` must be numeric.")
})

test_that("function returns correct plot", {
  plot <- plot_parliament(uk_parliament,
                  "Party",
                  "Seats",
                  "Percentage",
                  majorityLine = TRUE,
                  title = "2019 UK General Election",
                  subtitle = "Results",
                  size = 4,
                  legend = "bottom",
                  colours = colour_prep(uk_parliament, "Party", "polUK"),
  )

  expect_equal(plot$coordinates$ratio, 1)
  expect_equal(plot$labels$title, "2019 UK General Election")
  expect_equal(plot$guides$fill, "none")

})
