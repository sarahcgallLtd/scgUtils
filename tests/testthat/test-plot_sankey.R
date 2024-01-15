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
})

# Test with sample data
test_that("plot_sankey works with sample data", {
  expect_silent(plot_sankey(test_data, "source", "target", "value"))
})

# Test for output
test_that("sankey is correctly produced", {
  # Data
  df <- get_data("survey")
  df <- labelled::unlabelled(df)
  df <- df[, c("wt", "generalElectionVote", "p_past_vote_2019")]
  df <- grp_freq(df, c("generalElectionVote", "p_past_vote_2019"), "wt")

  result <- plot_sankey(df, "p_past_vote_2019", "generalElectionVote", "Freq",
              colours = colour_prep(df, c("generalElectionVote", "p_past_vote_2019"), pal_name = "polUK"),
              margin = list("left" = 0, "right" = 200))
  expect_type(result, "list")
})
