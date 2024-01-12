# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# ==============================================================#
test_that("all factor labels are retireved from the data frame", {
  x <- get_factor_labels(df)
  expect_equal(x$turnoutUKGeneral, "Likelihood to vote in general election")
})

test_that("all factor labels are added to new data frame", {
  new_df <- df
  # labels removed
  new_df[] <- lapply(new_df, function(x) if (is.factor(x)) factor(x) else x)

  x <- get_factor_labels(df)
  new_df <- add_attributes(new_df, x)

  expect_equal(attr(new_df[["turnoutUKGeneral"]], "label"), attr(df[["turnoutUKGeneral"]],"label"))
})

test_that("changes levels of factors", {
  x <- remove_levels(df)
  x <- levels(x$partyId)
  y <- levels(df$partyId)

  expect_false(isTRUE(all.equal(x, y)))
})