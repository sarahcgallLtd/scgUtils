# LOCAL TESTING
test_that("return a csv file from a local source", {
  file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
  csv_df <- get_file(file_path)
  expect_length(csv_df, 65)
  expect_equal(class(csv_df$turnoutUKGeneral), "character")
})

test_that("return a sav file from a local source", {
  file_path <- system.file("extdata", "survey.sav", package = "scgUtils")
  sav_df <- get_file(file_path)
  expect_length(sav_df, 65)
  expect_equal(class(sav_df$turnoutUKGeneral), c("haven_labelled","vctrs_vctr","double" ))
})

# ONE DRIVE TESTING
test_that("return a csv file from onedrive", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI", "Skipping this test in CI environment due to authentication required")
  csv_df <- get_file("scgUtils_testing/survey.csv", "onedrive")
  expect_length(csv_df, 65)
  expect_equal(class(csv_df$turnoutUKGeneral), "character")
})

test_that("return a sav file from onedrive", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI", "Skipping this test in CI environment due to authentication required")
  sav_df <- get_file("scgUtils_testing/survey.sav", "onedrive")
  expect_length(sav_df, 65)
  expect_equal(class(sav_df$turnoutUKGeneral), c("haven_labelled","vctrs_vctr","double" ))
})

# GOOGLE DRIVE TESTING


# WEB TESTING
test_that("return a csv file from web", {
  # Skip test if running in production
  skip_if(Sys.getenv("TEST_ENV") == "CI", "Skipping this test in CI environment due to download required")
  csv_df <- get_file("https://raw.githubusercontent.com/sarahcgall/scgUtils/master/inst/extdata/survey.csv", "web")
  expect_length(csv_df, 65)
  expect_equal(class(csv_df$turnoutUKGeneral), "character")
})