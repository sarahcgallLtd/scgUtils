# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  expect_error(plot_popn("data"), "A data frame is required to be parsed through this function.")

  expect_error(plot_popn(df, xVar = "column1"), "`xVar` variable must be a column in `data`.")

  expect_error(plot_popn(df, yVar = "column1"), "`yVar` variable must be a column in `data`.")

  expect_error(plot_popn(df, group = "column1"), "`group` variable must be a column in `data`.")

  expect_error(plot_popn(df, weight = "column1"), "`weight` variable must be a column in `data`.")
  expect_error(plot_popn(df, weight = "gender"), "`weight` must be numeric.")

  expect_error(plot_popn(df, meanVar = "column1"), "`meanVar` variable must be a column in `data`.")
  expect_error(plot_popn(df, meanVar = "gender"), "`meanVar` must be numeric.")
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
  x <- get_question(df,"turnoutUKGeneral")
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
    names(colour) <- c(leftLevel,rightLevel,"Total")
    return(colour)
  }

  x <- fn("Male","Female")
  expect_equal(x$Male, "#4682B4")

  x <- fn("Male","Female", colours=c("#000000","#ffffff","#eeeeee"))
  expect_equal(x$Male, "#000000")

})

test_that("data frame values converted to negative", {
  tmp <- data.frame(gender = c("Male","Female"),
                    Perc = c(1.1,2.2))

  x <- convert_neg(tmp, "gender", "Male", "Perc")
  expect_equal(x[1,2], -1.1) # Male
  expect_equal(x[2,2], 2.2) # Female
})

# ==============================================================#
# TEST: RESULT
