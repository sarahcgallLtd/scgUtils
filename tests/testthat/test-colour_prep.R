# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
df <- df[, c("wt", "generalElectionVote", "p_past_vote_2019")]
df <- grp_freq(df, c("generalElectionVote", "p_past_vote_2019"), "wt")
# ==============================================================#
test_that("error messages are correctly thrown", {
  expect_error(colour_prep(df, c("generalElectionVote", "invalid_column"), pal_name = "polUK"),
               "All specified columns must be present in the data frame.")
})

test_that("function works correctly", {
  colours <- colour_prep(df, c("generalElectionVote"), pal_name = "polUK")
  expect_type(colours, "list")

  colours <- colour_prep(df, c("generalElectionVote", "p_past_vote_2019"), pal_name = "polUK")
  expect_length(colours, 12)
  # Test exact match
  expect_equal(colours$Conservative, "#2e84c6")
  # Test partial reverse match
  expect_equal(colours$`Scottish National Party (SNP)`, "#FFF95D")
})
