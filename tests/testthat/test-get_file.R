# LOCAL TESTING
# test_that("return a csv file from a local source", {
#   file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
#   csv_df <- get_file(file_path)
#   expect_length(csv_df, 65)
#   expect_equal(class(csv_df$turnoutUKGeneral), "character")
# })
#
# test_that("return a sav file from a local source", {
#   file_path <- system.file("extdata", "survey.sav", package = "scgUtils")
#   sav_df <- get_file(file_path)
#   expect_length(sav_df, 65)
#   expect_equal(class(sav_df$turnoutUKGeneral), "factor")
# })

test_that("return error message for non existent file on a local source", {
  expect_error(get_file("invalid_file_path"), "Unsupported file type or file does not exist. check `file_path` and try again.")
})

# test_that("return error message for unsupported file source", {
#   file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
#   expect_error(get_file(file_path, "invalid_source"), "'arg' should be one of")
# })

# test_that("return error message for unsupported file source", {
#   file_path <- system.file("data","survey.rda", package = "scgUtils")
#   expect_error(get_file(file_path), "Unsupported file type")
# })

# ONE DRIVE TESTING
test_that("return a csv file from onedrive", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI",
          "Skipping this test in CI environment due to authentication required")
  csv_df <- get_file("scgUtils_examples_folder/survey.csv", "onedrive")
  expect_length(csv_df, 65)
  expect_equal(class(csv_df$turnoutUKGeneral), "character")
})

test_that("return a sav file from onedrive", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI",
          "Skipping this test in CI environment due to authentication required")
  sav_df <- get_file("scgUtils_examples_folder/survey.sav", "onedrive")
  expect_length(sav_df, 65)
  expect_equal(class(sav_df$turnoutUKGeneral), "factor")
})

# GOOGLE DRIVE TESTING
# test_that("return a csv file from googledrive", {
#   # Skip test if running in production
#   skip_if(Sys.getenv("TEST_ENV") == "CI",
#           "Skipping this test in CI environment due to authentication required")
#   csv_df <- get_file("survey.csv", "googledrive")
#   expect_length(csv_df, 65)
#   expect_equal(class(csv_df$turnoutUKGeneral), "character")
# })

# WEB TESTING
test_that("return a csv file from web", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI",
          "Skipping this test in CI environment due to download required")
  csv_df <- get_file("https://raw.githubusercontent.com/sarahcgallLtd/scgUtils/master/inst/extdata/survey.csv", "web")
  expect_length(csv_df, 65)
  expect_equal(class(csv_df$turnoutUKGeneral), "character")
})