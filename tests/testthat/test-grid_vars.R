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
  expect_error(grid_vars("data"), "A data frame is required to be parsed through this function.")

  expect_error(grid_vars(df, "vars"), "`vars` must be a list.")
  var <- list("column1")
  expect_error(grid_vars(df, var), "`vars` list must contain column names.")
  var <- list(column = "column1")
  expect_error(grid_vars(df, var), "`vars` list must contain columns from `data`.")

  expect_error(grid_vars(df, vars, group = "column1"), "`group` variable must be a column in `data`.")

  expect_error(grid_vars(df, vars, weight = "column1"), "`weight` variable must be a column in `data`.")
  expect_error(grid_vars(df, vars, weight = "gender"), "`weight` must be numeric.")
})

# ==============================================================#
# TEST: PREPARE VARIABLES
test_that("list of variables becomes a vector", {
  x <- dput(names(vars), file = nullfile())  # suppress output to console

  expect_equal(x, c("likeSunak", "likeStarmer"))

  fn <- function(data, vars, group, weight) {
    y <- append_if_exists(as.list(match.call()[-1])[-c(1, 2)])
  }

  expect_equal(fn(df, vars, "gender", "wt"), c("gender", "wt"))
  expect_equal(fn(df, vars, "gender"), "gender")
  expect_equal(fn(df, vars, weight = "wt"), "wt")
  expect_equal(fn(df, vars), NULL)

  fn <- function(data, vars, group, weight) {
    grp <- match.call(expand.dots = FALSE)
    y <- append_if_exists(as.list(grp[match("group", names(grp), 0)]))
  }

  expect_equal(fn(df, vars, "gender", "wt"), "gender")
  expect_equal(fn(df, vars, "gender"), "gender")
  expect_equal(fn(df, vars, weight = "wt"), NULL)
  expect_equal(fn(df, vars), NULL)
})

# ==============================================================#
# TEST: TRANSFORM DATA
test_that("vars list in Question column are renamed to new names", {

  # base r
  fn1 <- function(data, vars, group, weight) {
    x <- dput(names(vars), file = nullfile())
    y <- c(append_if_exists(as.list(match.call()[-1])[-c(1, 2)]), x)
    tmp <- data[, y]
    tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)
    tmp <- tidyr::pivot_longer(tmp, cols = names(tmp[,x]), names_to = "Question", values_to = "Response")
    tmp$Question <-  unlist(vars)[tmp$Question]
    return(tmp)
  }

  # dplyr
  fn2 <- function(data, vars, group, weight) {
    x <- dput(names(vars), file = nullfile())
    y <- c(append_if_exists(as.list(match.call()[-1])[-c(1, 2)]), x)
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
