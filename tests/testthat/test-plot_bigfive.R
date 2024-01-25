# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  expect_error(plot_bigfive(data="data"),
               "Parameter `data` is required and must be a data frame.")
  # big_five ===================================================#
  expect_error(plot_bigfive(df, bigfive = c("column1", "column2")),
               "`bigfive` variable must be a column in `data`.")
  # expect_error(plot_bigfive(df, bigfive = c("gender","partyId")), "`bigfive` must be numeric (0-100).")

  # group ======================================================#
  expect_error(plot_bigfive(df, group = "column1"),
               "`group` must be a column in `data`.")

  # weight =====================================================#
  expect_error(plot_bigfive(df, weight = "column1"),
               "`weight` must be a column in `data`.")
  expect_error(plot_bigfive(df, weight = "gender"),
               "`weight` must be numeric.")
})

# ==============================================================#
# TEST: CHECK RESULTS
test_that("function produces graph", {
  tmp <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female"),
                    Weight = c(0.6, 0.8, 0.9, 1.0, 1.3, 1.7, 1.0, 0.99),
                    Neuroticism = c(60, 40, 30, 80, 20, 25, 50, 10),
                    Extroversion = c(75, 20, 35, 45, 50, 10, 60, 90),
                    Openness = c(50, 50, 45, 30, 65, 80, 10, 55),
                    Agreeableness = c(90, 30, 50, 20, 10, 75, 65, 35),
                    Conscientiousness = c(45, 50, 90, 10, 25, 30, 80, 40))

  p <- plot_bigfive(tmp, c("Neuroticism", "Extroversion", "Openness", "Agreeableness", "Conscientiousness"), weight = "Weight")
  expect_equal(length(p$layers), 6)

  p <- plot_bigfive(tmp, c("Neuroticism", "Extroversion", "Openness", "Agreeableness", "Conscientiousness"), group = "Gender")
  expect_equal(length(p$layers), 8)

  tmp <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female"),
                    Weight = c(0.6, 0.8, 0.9, 1.0, 1.3, 1.7, 1.0, 0.99),
                    Neuroticism = c(60, 40, 30, 80, 20, 25, 50, 10),
                    Extroversion = c(75, 20, 35, 45, 50, 10, 60, 90),
                    Openness = c(50, 50, 45, 30, 65, 80, 10, 55),
                    Agreeableness = c(90, 30, 50, 20, 10, 75, 65, 35),
                    Conscientiousness = c(45, 50, 90, 10, 25, 30, 80, 40))

  tmp$Gender <- factor(tmp$Gender)

  p <- plot_bigfive(tmp, c("Neuroticism", "Extroversion", "Openness", "Agreeableness", "Conscientiousness"), weight = "Weight")
  expect_equal(length(p$layers), 6)

  p <- plot_bigfive(tmp, c("Neuroticism", "Extroversion", "Openness", "Agreeableness", "Conscientiousness"), group = "Gender")
  expect_equal(length(p$layers), 8)

  tmp <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female"),
                    Weight = c(0.6, 0.8, 0.9, 1.0, 1.3, 1.7, 1.0, 0.99),
                    Neuroticism = c(60, 40, 30, 80, 20, 25, 50, 110),
                    Extroversion = c(75, 20, 35, 45, 50, 10, 60, 90),
                    Openness = c(50, 50, 45, 30, 65, 80, 10, 55),
                    Agreeableness = c(90, 30, 50, 20, 10, 75, 65, 35),
                    Conscientiousness = c(45, 50, 90, 10, 25, 30, 80, 40))
  expect_error(plot_bigfive(tmp, c("Neuroticism", "Extroversion", "Openness", "Agreeableness", "Conscientiousness"), group = "Gender"),
               "`Neuroticism` values must be between 0 and 100.")
})
