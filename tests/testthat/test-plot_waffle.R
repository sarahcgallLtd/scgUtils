# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

test_that("function returns correct errors", {
  expect_error(plot_waffle("data"),
               "Parameter `data` is required and must be a data frame.")
  expect_error(plot_waffle(df, "generalElectionVote", values = "invalid_column"),
               "`values` must be a column in `data`.")
  expect_error(plot_waffle(df, "invalid_column"),
               "`group` must be a column in `data`.")
  expect_error(plot_waffle(df, "generalElectionVote", values = "generalElectionVote"),
               "`values` must be numeric.")
  expect_error(plot_waffle(df, "generalElectionVote", weight = "generalElectionVote"),
               "`weight` must be numeric.")
  expect_error(plot_waffle(df, "p_socgrade", isolateVar = "invalid_variable"),
               "`isolateVar` must be in `group`.")
})

test_that("function correctly returns plot", {
  x <- plot_waffle(df, "generalElectionVote", weight = "wt", title = "generalElectionVote",
                   varColours = colour_prep(df, "generalElectionVote", pal_name = "polUK"),
                   orderPlots = "ascending", isolateVar = "Conservative")
  expect_equal(x$plot_env$varColours$Conservative, "#2e84c6")

  x <- plot_waffle(df %>% filter(p_socgrade != "Unknown"), "p_socgrade", weight = "wt", title = "p_socgrade")
  expect_equal(x$labels$title, "Social Grade")

  x <- plot_waffle(df, "p_disability", weight = "wt", orderPlots = "descending")
  expect_equal(x$facet$params$strip.position, "top")

  data <- data.frame(
    Category = c("A", "B", "C"),
    Count = c(30, 40, 30)
  )

  x <- plot_waffle(data, "Category", values = "Count", isolateVar = "A")
  expect_equal(x$labels$fill, "category")

})