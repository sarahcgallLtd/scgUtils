colours <- get0("colours", envir = asNamespace("scgUtils"))

# ==============================================================#
# INDIVIDUAL COLOUR
test_that("returns colour, palette, and error", {

  fn <- function(colours, pal_name, n) {
    if (pal_name %in% colours$name) {
      pal <- colours[colours$name == pal_name,]
      pal <- pal[["colour"]]
      return(pal)

    } else if (pal_name %in% colours$palette) {
      pal <- colours[colours$palette == pal_name,]
      return(pal)

    } else
      stop("Palette not found.")
  }

  p <- colour_pal("Jaffa")
  expect_equal(p, "#e78e47")

  p <- fn(colours, "polAus")
  expect_equal(p[1, 3], "#de2b33")

  expect_error(colour_pal("Jaffal"), "Palette not found")

})

# ==============================================================#
# PALETTE COLOURS
test_that("returns palette", {

  fn <- function(colours, pal_name, n) {
    pal <- colours[colours$palette == pal_name,]

    if (missing(n)) {
      n <- max(pal$number)
    }

    if (grepl("^pol", pal_name) == FALSE) {
      if (grepl("^seq", pal_name) == TRUE || grepl("^div", pal_name) == TRUE) {
        if (missing(n)) {
          n <- max(pal$number)
        }
        pal <- pal[pal$number == n,]
      }
      pal <- pal[, c("value", "colour")]
      names(pal)[names(pal) == "value"] <- "name"

    } else
      pal <- pal[, c("name", "colour")]

    if (nrow(pal) == 0) {
      stop("Number of requested colours is greater than what this palette can offer.")
    }

    return(pal)
  }

  p <- fn(colours, "polAus")
  expect_equal(p[1, 2], "#de2b33")
  expect_error(colour_pal("seqBlue", 8), "Number of requested colours is greater than what this palette can offer.")
})

# ==============================================================#
# RETURN COLOURS
test_that("function returns colour palettes in the correct format", {
  p <- colour_pal("divRedBlue", n = 4, assign = c("hi1", "hi2", "hi3", "hi4"))
  expect_equal(class(p), "list")

  p <- colour_pal("polAus", assign = c("hi1", "hi2", "hi3", "hi4","hi5","hi6","hi7"))
  expect_equal(class(p), "list")

  p <- colour_pal("polAus")
  expect_equal(class(p), "list")

  p <- colour_pal("catSimplified")
  expect_equal(class(p), "character")

  p <- colour_pal("catSimplified", type = "discrete_as")
  expect_equal(class(p), "list")

  p <- colour_pal("seqBlue", type = "continuous")
  expect_equal(class(p), "function")
})
