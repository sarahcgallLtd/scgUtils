# ==============================================================#
test_that("function works correctly with factors", {
  # Data
  df <- get_data("survey")
  df <- labelled::unlabelled(df)
  df <- process_factors(df)

  rowVars <- c("turnoutUKGeneral", "generalElectionVote", "partyIdStrength")
  colVars <- c("gender", "ageGroup", "partyId", "p_socgrade")

  x <- compile(df, rowVars, colVars, "wt", save = FALSE)
  expect_length(x, 27)

  x <- compile(df, rowVars, colVars, "wt", save = FALSE, format = "statistics")
  expect_length(x, 7)

  x <- compile(df, rowVars, colVars, "wt")
  expect_null(x)
})

test_that("function works correctly with characters", {
  # Data
  file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
  df1 <- get_file(file_path)

  rowVars <- c("turnoutUKGeneral", "generalElectionVote", "partyIdStrength")
  colVars <- c("gender", "ageGroup", "partyId", "p_socgrade")

  x <- compile(df1, rowVars, colVars, "wt", save = FALSE)
  expect_length(x, 27)

  x <- compile(df1, rowVars, colVars, "wt", save = FALSE, format = "statistics")
  expect_length(x, 7)

  x <- compile(df1, rowVars, colVars, "wt")
  expect_null(x)
})
