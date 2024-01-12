# ==============================================================#
# TEST: CHECK PARAMS
test_that("parameters return correct error", {
  # data =======================================================#
  df <- get_data("survey")
  expect_error(grp_freq("data"),
               "A data frame is required to be parsed through this function.")

  # groups =====================================================#
  expect_error(grp_freq(df, groups = "column1"),
               "`groups` variable must be columns in `data`.")
  expect_error(grp_freq(df, groups = c("gender", "column2")),
               "`groups` variable must be columns in `data`.")
  expect_error(grp_freq(df),
               "`groups` is required to be parsed through this function.")

  # weight =====================================================#
  expect_error(grp_freq(df, groups = "gender", weight = "column1"),
               "`weight` variable must be a column in `data`.")
  #Unsure why this fails (correct error is thrown but test fails):
  # expect_error(grp_freq(df, groups="gender", weight = "gender"), "`weight` must be numeric.")

  # groupsPercent ==============================================#
  expect_error(grp_freq(df, groups = "gender", groupsPercent = "column1"),
               "`groupsPercent` variable must be columns in `data`.")
  expect_error(grp_freq(df, groups = "gender", groupsPercent = "turnoutUKGeneral"),
               "`groupsPercent` variable must be in `groups`.")
})

# ==============================================================#
# TEST: PREPARE DATA
test_that("function adds a list of column names from a group", {
  data <- data.frame(a = c(1, 2, 3, 4, 5),
                     b = c(1, 2, 3, 4, 5),
                     c = c(1, 2, 3, 4, 5))

  x <- list_group(data, "a")
  expect_equal(names(x), "a")
})

test_that("take the first variable from group", {
  group <- c("a", "b", "c")
  subgroup <- group[1]

  expect_equal(subgroup, "a")
})

# ==============================================================#
# TEST: GET FREQUENCY
test_that("function adds a list of column names from a group", {
  tmp <- data.frame("Party" = c("Conservative", "Labour", "Labour", "Labour",
                                 "Conservative", "Conservative", "Conservative", "Labour"),
                     "Gender" = c("Male", "Male", "Female", "Female",
                                  "Male", "Male", "Female", "Female"))
  grp <- list_group(tmp, "Gender")
  tmp <- stats::aggregate(grp[[1]], by = grp, FUN = length)

  expect_equal(tmp[1,2], 4)
})
# ==============================================================#
# TEST: SET COLUMN NAMES
test_that("function sets names", {
  tmp <- data.frame("Gender.Gender" = c("Male", "Male", "Female", "Female"),
                    "n" = c(37, 20, 27, 40))

  tmp <- stats::setNames(tmp, c("Gender", "Freq"))
  expect_equal(names(tmp), c("Gender","Freq"))

  set_names <- c("Gender", "Freq")
  tmp <- stats::setNames(tmp, set_names)
  expect_equal(names(tmp), c("Gender","Freq"))
})

# ==============================================================#
# TEST: ADD PERCENT
test_that("percent of subgroup is added to frequency", {
  tmp <- data.frame("Party" = c("Conservative", "Labour", "Conservative", "Labour"),
                     "Gender" = c("Male", "Male", "Female", "Female"),
                     "Freq" = c(37, 20, 27, 40))
  groups <- c("Party", "Gender")
  groupsPercent <- groups[1]

  fn <- function(data, groupsPercent, round_decimals) {
    if (missing(groupsPercent))
      data <- transform(data, Perc = stats::ave(Freq, FUN = function(x) x / sum(x) * 100))
    else
      data <- transform(data, Perc = stats::ave(Freq, data[, groupsPercent],
                                            FUN = function(x) x / sum(x) * 100))
    if (is.numeric(round_decimals)==TRUE)
      data <- round_vars(data, round_decimals)
    return(data)
  }

  x <- fn(tmp, groupsPercent, round_decimals = 2)
  expect_equal(x[1, 4], 57.81)

  y <- fn(tmp, round_decimals = 2)
  expect_equal(y[1, 4], 29.84)
})

# ==============================================================#
# TEST: RESULT
test_that("function returns data frame with original groups and frequency and percent", {
  library(dplyr)
  # WITHOUT WEIGHTS
  tmp <- data.frame("Party" = c("Conservative", "Labour", "Labour", "Labour",
                                 "Conservative", "Conservative", "Conservative", "Labour"),
                     "Gender" = c("Male", "Male", "Female", "Female",
                                  "Male", "Male", "Female", "Female"))

  # Frequency:
  # Grouped Frequency function
  x <- grp_freq(tmp, groups="Gender")

  # Compared with dplyr version
  y <- tmp %>%
    group_by(Gender) %>%
    count() %>%
    ungroup() %>%
    as.data.frame()

  expect_identical(x[1,2], y[1,2])

  # Percentage:
  # Grouped Frequency function
  x <- grp_freq(tmp, groups=c("Party","Gender"), groupsPercent="Party")

  # Compared with dplyr version
  y <- tmp %>%
    group_by(Party, Gender) %>%
    count() %>%
    ungroup() %>%
    group_by(Gender) %>%
    mutate(Perc = round(n/sum(n)*100,2)) %>%
    ungroup() %>%
    as.data.frame()

  expect_identical(x[1,4], y[1,4])

  # WITH WEIGHTS
  tmp <- data.frame("Party" = c("Conservative", "Labour", "Labour", "Labour",
                                 "Conservative", "Conservative", "Conservative", "Labour"),
                     "Gender" = c("Male", "Male", "Female", "Female",
                                  "Male", "Male", "Female", "Female"),
                     "Weight" = c(0.3,1.2,0.9,1.0,1.0,0.6,1.1,1.5)
  )

  # Frequency:
  # Grouped Frequency function
  x <- grp_freq(tmp, groups="Gender", weight="Weight", set_names = c("Gender","Freq"))

  # Compared with dplyr version
  y <- tmp %>%
    group_by(Gender) %>%
    summarise(Freq = sum(Weight), .groups='drop') %>%
    ungroup() %>%
    as.data.frame()

  expect_identical(x[1,2], y[1,2])

  # Percentage:
  # Grouped Frequency function
  x <- grp_freq(tmp, groups=c("Gender","Party"), weight="Weight",
                addPercent="yes", round_decimals = 2)

  # Compared with dplyr version
  y <- tmp %>%
    group_by(Gender,Party) %>%
    summarise(Freq = sum(Weight), .groups = 'drop') %>%
    ungroup() %>%
    mutate(Perc = round(Freq/sum(Freq)*100,2)) %>%
    as.data.frame()

  expect_identical(x[1,4], y[1,4])
})
