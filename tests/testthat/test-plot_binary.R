# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
# Vars
vars <- list(p_parent = "Parent")
# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  expect_error(plot_binary("data"),
               "Parameter `data` is required and must be a data frame.")

  # vars =======================================================#
  expect_error(plot_binary(df, "vars"),
               "`vars` must be a column in `data`.")
  var <- list("column1")
  expect_error(plot_binary(df, var),
               "`vars` must be a non-empty list with named elements.")
  var <- list(column = "column1")
  expect_error(plot_binary(df, var),
               "`vars` must be a column in `data`.")
  var <- list(likeSunak = "Sunak")
  expect_error(plot_binary(df, var, value = "Strongly like"),
               "`vars` variables must contain binary values only")

  # group ======================================================#
  expect_error(plot_binary(df, vars, group = "column1"),
               "`group` must be a column in `data`.")

  # weight =====================================================#
  expect_error(plot_binary(df, vars, weight = "column1"),
               "`weight` must be a column in `data`.")
  expect_error(plot_binary(df, vars, weight = "gender"),
               "`weight` must be numeric.")

  # value ======================================================#
  expect_error(plot_binary(df, vars, value = 12),
               "`value` must be a single character string.")
  expect_error(plot_binary(df, vars, value = c("Something Else", "Something")),
               "`value` must be a single character string.")
  expect_error(plot_binary(df, vars, value = "Something Else"),
               "`vars` variables must contain binary values only, and `value` must be one of these.")
})

# ==============================================================#
# TEST: RESULTS
test_that("function produces correct equation", {

  fn <- function(data, vars, value, group, weight) {
    bin <- match.call(expand.dots = FALSE)

    # Limit to data, vars, and weight
    bin_n <- match(c("data", "vars", "weight"), names(bin), 0L)
    bin <- bin[c(1L, bin_n)]

    # Substitute 'plot_binary' for 'grid_vars'
    bin[[1L]] <- quote(grid_vars)

    return(as.character(bin))
  }

  expect_equal(fn(df, vars), c("grid_vars", "df", "vars"))

})


# ==============================================================#
# TEST: RESULTS
test_that("function produces graph (factors)", {
  # Total
  p <- plot_binary(df, vars, "Yes")
  expect_equal(p$labels$hjust, "hjust")

  # Grouped
  p <- plot_binary(df, vars, "Yes", "partyId")
  expect_equal(p$labels$group, "id")
})

test_that("function produces graph (character)", {
  file_path <- system.file("extdata", "survey.csv", package = "scgUtils")
  df1 <- get_file(file_path)
  # Total
  p <- plot_binary(df1, vars, "Yes")
  expect_equal(p$labels$hjust, "hjust")

  # Grouped
  p <- plot_binary(df1, vars, "Yes", "partyId")
  expect_equal(p$labels$group, "id")
})