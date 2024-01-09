# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)
# Vars
vars <- list(p_parent = "Parent")
# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  expect_error(plot_binary("data"), "A data frame is required to be parsed through this function.")

  expect_error(plot_binary(df, "vars"), "`vars` must be a list.")
  var <- list("column1")
  expect_error(plot_binary(df, var), "`vars` list must contain column names.")
  var <- list(column = "column1")
  expect_error(plot_binary(df, var), "`vars` list must contain columns from `data`.")

  expect_error(plot_binary(df, vars, group = "column1"), "`group` variable must be a column in `data`.")

  expect_error(plot_binary(df, vars, weight = "column1"), "`weight` variable must be a column in `data`.")
  expect_error(plot_binary(df, vars, weight = "gender"), "`weight` must be numeric.")

  expect_error(plot_binary(df, vars, value = 12), "`value` must be a character string.")
  expect_error(plot_binary(df, vars, value = c("Something Else", "Something")), "`value` must be a single character string only")

  expect_error(plot_binary(df, vars, value = "Something Else"), "`value` must be in `vars`.")

  var <- list(likeSunak = "Sunak")
  expect_error(plot_binary(df, var, value = "Strongly like"), "`vars` variables must contain binary values only")
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
test_that("function produces graph)", {
  # Total
  p <- plot_binary(df, vars, "Yes")
  expect_equal(p$labels$hjust, "hjust")

  # Grouped
  p <- plot_binary(df, vars, "Yes", "partyId")
  expect_equal(p$labels$group, "id")
})
