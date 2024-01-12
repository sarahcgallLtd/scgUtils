# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
df <- remove_levels(df)

rowVars <- c("turnoutUKGeneral", "generalElectionVote", "partyIdStrength")
colVars <- c("gender", "ageGroup", "partyId", "p_socgrade")

# ==============================================================#
test_that("function works correctly", {
  x <- compile(df, rowVars, colVars, "wt", save = FALSE)
  expect_length(x, 27)

  x <- compile(df, rowVars, colVars, "wt", save = FALSE, format = "statistics")
  expect_length(x, 7)

  x <- compile(df, rowVars, colVars, "wt")
  expect_null(x)
})
