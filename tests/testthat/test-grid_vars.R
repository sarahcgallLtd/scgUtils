# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
# Vars
vars <- list(likeSunak = "Sunak",
             likeStarmer = "Starmer"
)
# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  expect_error(grid_vars("data"),
               "Parameter `data` is required and must be a data frame.")
  # vars =======================================================#
  expect_error(grid_vars(df, "vars"),
               "`vars` must be a non-empty list with named elements.")
  var <- list("column1")
  expect_error(grid_vars(df, var),
               "`vars` must be a non-empty list with named elements.")
  var <- list(column = "column1")
  expect_error(grid_vars(df, var),
               "`vars` must be a column in `data`.")

  # group ======================================================#
  expect_error(grid_vars(df, vars, group = "column1"),
               "`group` must be a column in `data`.")

  # weight =====================================================#
  expect_error(grid_vars(df, vars, weight = "column1"),
               "`weight` must be a column in `data`.")
  expect_error(grid_vars(df, vars, weight = "gender"),
               "`weight` must be numeric.")
})

# ==============================================================#
# TEST: PREPARE VARIABLES
test_that("list of variables becomes a vector", {
  x <- dput(names(vars), file = nullfile())  # suppress output to console

  expect_equal(x, c("likeSunak", "likeStarmer"))

  fn <- function(group=NULL, weight=NULL) {
    y <- append_if_exists(group, weight)
    return(y)
  }

  expect_equal(fn("gender", "wt"), c("gender", "wt"))
  expect_equal(fn("gender"), "gender")
  expect_equal(fn(weight = "wt"), "wt")
  expect_equal(fn(), NULL)
})

# ==============================================================#
# TEST: TRANSFORM DATA
test_that("vars list in Question column are renamed to new names", {

  # base r
  fn1 <- function(data, vars, group, weight) {
    x <- dput(names(vars), file = nullfile())
    y <- c(append_if_exists(group), x)
    tmp <- data[, y]
    tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)
    tmp <- tidyr::pivot_longer(tmp, cols = names(tmp[,x]), names_to = "Question", values_to = "Response")
    tmp$Question <-  unlist(vars)[tmp$Question]
    return(tmp)
  }

  # dplyr
  fn2 <- function(data, vars, group, weight) {
    x <- dput(names(vars), file = nullfile())
    y <- c(append_if_exists(group), x)
    tmp <- data[, y]
    tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)
    tmp <- tidyr::pivot_longer(tmp, cols = names(tmp[,x]), names_to = "Question", values_to = "Response")
    tmp <- dplyr::mutate(tmp, Question = dplyr::coalesce(unlist(vars)[Question], Response))
    return(tmp)
  }

  x <- fn1(df, vars, group = "gender")
  y <- fn2(df, vars, group = "gender")

  expect_equal(y, y)

})
# ==============================================================#
# GET GROUPED FREQUENCY & PERCENTAGE


# ==============================================================#
# TEST: RESULT
test_that("function returns data frame with original groups and frequency and percent", {
  # WITHOUT WEIGHTS
  x <- grid_vars(df, vars, group = "gender")
  expect_length(x, 5)
  expect_equal(x[1, 4], 600)
  expect_equal(x[1, 5], 23.04)

  # WITH WEIGHTS
  x <- grid_vars(df, vars, group = "gender", weight = "wt")
  expect_length(x, 5)
  expect_equal(x[1, 4], 511.98)
  expect_equal(x[1, 5], 23.79)

  # WITHOUT GROUP
  x <- grid_vars(df, vars)
  expect_length(x, 4)
  expect_equal(x[1, 3], 1055)
  expect_equal(x[1, 4], 21.1)

  # WITHOUT GROUP
  x <- grid_vars(df, vars, weight = "wt")
  expect_length(x, 4)
  expect_equal(x[1, 3], 850.59)
  expect_equal(x[1, 4], 21.31)
})
