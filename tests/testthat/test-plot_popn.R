# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("plot_popn function handles parameter checks", {
  # Test case 1: Invalid 'data' parameter type
  expect_error(plot_popn(data = "invalid_data", xVar = "gender", yVar = "ageGroup"),
               "Parameter `data` is required and must be a data frame.")

  # Test case 2: Invalid 'xVar' or 'yVar' not present in data
  expect_error(plot_popn(data = df, xVar = "invalid_column", yVar = "ageGroup"),
               "`xVar` must be a column in `data`.")

  # Test case 3: Invalid 'xVar' or 'yVar' not present in data (numeric)
  expect_error(plot_popn(data = df, xVar = "gender", yVar = "invalid_column"),
               "`yVar` must be a column in `data`.")

  # Test case 4: Invalid 'meanVar' data (numeric)
  expect_error(plot_popn(data = df, xVar = "gender", yVar = "ageGroup", meanVar = "turnoutUKGeneral"),
               "`meanVar` must be numeric.")
})

# ==============================================================#
# PREPARE VARIABLES
test_that("correct levels are returned", {
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  # Get `x_var` levels
  xLevels <- levels(df[["gender"]])

  # first level
  expect_equal(xLevels[[1]], "Male")
  # second level
  expect_equal(xLevels[[2]], "Female")

  # Get number of `y_var` levels
  yLevels <- length(unique(df[["ageGroup"]]))
  expect_equal(yLevels, 6)

})

test_that("question is returned", {
  x <- get_question(df, "turnoutUKGeneral")
  expect_equal(x, "Likelihood to vote in general election")
})

# ==============================================================#
# PREPARE COLOURS
test_that("colours are returned in a list", {

  fn <- function(leftLevel, rightLevel, colours) {
    if (missing(colours)) {
      colour <- list(colour_pal("Steel Blue"),
                     colour_pal("Lilac"),
                     colour_pal("French Grey")
      )
    } else {
      colour <- list(colours[[1]],
                     colours[[2]],
                     colours[[3]]
      )
    }
    names(colour) <- c(leftLevel, rightLevel, "Total")
    return(colour)
  }

  x <- fn("Male", "Female")
  expect_equal(x$Male, "#4682B4")

  x <- fn("Male", "Female", colours = c("#000000", "#ffffff", "#eeeeee"))
  expect_equal(x$Male, "#000000")

})

test_that("data frame values converted to negative", {
  tmp <- data.frame(gender = c("Male", "Female"),
                    Perc = c(1.1, 2.2))

  x <- convert_neg(tmp, "gender", "Male", percCol="Perc")
  expect_equal(x[1, 2], -1.1) # Male
  expect_equal(x[2, 2], 2.2) # Female
})

# ==============================================================#
# TEST: RESULT
test_that("function produces correct graph", {
  # file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
  # df1 <- get_file(file_path)
  # # Basic
  # p <- plot_popn(data = df1,
  #                xVar = "gender",
  #                yVar = "ageGroup",
  #                title = "Population Structure",
  #                xLab = "Population (%)",
  #                yLab = "Age")
  #
  # expect_identical(p$labels$title, "Population Structure")
  # expect_identical(p$labels$x, "Population (%)")
  # expect_identical(p$labels$y, "Age")

  # total with mean
  p <- plot_popn(data = df,
                 xVar = "gender",
                 yVar = "ageGroup",
                 weight = "wt",
                 meanVar = "age",
                 addLabels = "yes",
                 faceLab = "bold")

  expect_identical(p$coordinates$clip, "off")


  # Weighted with group
  p <- plot_popn(data = df,
                 xVar = "gender",
                 yVar = "ageGroup",
                 group = "partyId",
                 weight = "wt")

  expect_identical(p$labels$subtitle, "Party identification")

  # p <- plot_popn(data=df1,
  #         xVar="gender",
  #         yVar="ageGroup",
  #         weight="wt",
  #         group="partyId",
  #         addLabels = "yes",
  #         thresholdLab = 5,
  #         colour = c("#Bf8AB6","#4682B4","#cdcdd1")
  # )
  # expect_identical(p$facet$params$as.table, TRUE)
})
