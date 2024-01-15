# Sample data for testing
test_data <- data.frame(
  source = c("A", "B", "A", "C"),
  target = c("X", "Y", "Z", "X"),
  value = c(10, 20, 30, 40)
)
# ==============================================================#
# Test for correct parameter handling
test_that("plot_sankey stops with incorrect inputs", {
  expect_error(plot_sankey(test_data, "source", "target", "nonexistent_column"),
               "value columns must be present in the data")
  expect_error(plot_sankey(test_data, source = "source", target = "target", value = "value", colours = "not_a_colour_vector"),
               "Invalid colour name: not_a_colour_vector")
  colours <- data.frame(names = c("A", "B", "C", "X", "Y", "Z"),
                        colours = c("#4ae2da", "#e78e47", "#20435b", "#8c9aa4", "#cdcdd1", "#4682B4"))
  expect_error(plot_sankey(test_data, source = "source", target = "target", value = "value", colours = colours),
               "Colours must be a list or a vector.")
  expect_error(plot_sankey(test_data, "source", "target", "value", shiftLabel = "x"),
               "shiftLabel must be a numeric value only")
})

# Test with sample data
test_that("plot_sankey works with sample data", {
  expect_silent(plot_sankey(test_data, "source", "target", "value"))
})

# Test for output
test_that("sankey is correctly produced", {
  result <- plot_sankey(test_data, "source", "target", "value", shiftLabel = 4,
                        colours = c("yellow", "blue", "pink", "green", "black", "purple", "red", "white"))
  # Can't test colourScale properly due to actual vs expected being different on github vs locally
  expect_type(result$x$options$colourScale, "character")

  # Data
  df <- get_data("survey")
  df <- labelled::unlabelled(df)
  df <- df[, c("wt", "generalElectionVote", "p_past_vote_2019")]
  df <- grp_freq(df, c("generalElectionVote", "p_past_vote_2019"), "wt")

  result <- plot_sankey(df, "p_past_vote_2019", "generalElectionVote", "Freq",
                        colours = colour_prep(df, c("generalElectionVote", "p_past_vote_2019"), pal_name = "polUK"),
                        margin = list("left" = 0, "right" = 200))
  expect_type(result, "list")
  expect_equal(result$x$options$fontSize, 20)
  expect_equal(result$width, 1200)
})
