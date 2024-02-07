# Data
df <- get_data("survey")
df <- labelled::unlabelled(df)

# Vars
vars <- list(likeSunak = "Sunak",
             likeStarmer = "Starmer",
             likeCon = "Conservative",
             likeLab = "Labour",
             likeLD = "Lib Dems",
             likeSNP = "SNP",
             likePC = "Plaid",
             likeBrexitParty = "Reform",
             likeGrn = "Greens"
)

varLevels_vector <- c("Strongly dislike", "1", "2", "3", "4", "5", "Don't know", "6", "7", "8", "9", "Strongly like")
varLevels_list <- list(left = c("Strongly dislike", "1", "2", "3", "4", "5"),
                       neutral = c("Don't know"),
                       right = c("6", "7", "8", "9", "Strongly like"))

colours <- colour_pal("divRedBlue", 11, c("Strongly dislike", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Strongly like"))
colours$`Don't know` <- "grey90"

# ==============================================================#
test_that("correct errors are thrown", {
  expect_error(plot_likert("data"),
               "Parameter `data` is required and must be a data frame.")
  expect_error(plot_likert(df, "invalid_vars"),
               "`vars` must be a column in `data`.")
  expect_error(plot_likert(df, vars, group = "invalid_column"),
               "`group` must be a column in `data`.")
  expect_error(plot_likert(df, vars, weight = "invalid_column"),
               "`weight` must be a column in `data`.")
  expect_error(plot_likert(df, vars, weight = "gender"),
               "`weight` must be numeric.")
  expect_error(plot_likert(df, vars, varLevels = matrix("invalid_levels")),
               "`varLevels` must be either a vector or a list with 'left', 'neutral', and 'right' elements.")
  # ==============================================================#
  # checking vector type in varLevels
  misspelt <- c("Strongly dislike", "1", "2", "3", "4", "5", "Dont know", "6", "7", "8", "9", "Strongly like")
  expect_error(plot_likert(df, vars, varLevels = misspelt),
               "The following levels in `varLevels` are not present in the column 'likeSunak': Dont know.")
  additional <- c("Another_level", "Strongly dislike", "1", "2", "3", "4", "5", "Don't know", "6", "7", "8", "9", "Strongly like")
  expect_error(plot_likert(df, vars, varLevels = additional),
               "The following levels in `varLevels` are not present in the column 'likeSunak': Another_level.")
  missing <- c("1", "2", "3", "4", "5", "Don't know", "6", "7", "8", "9", "Strongly like")
  expect_error(plot_likert(df, vars, varLevels = missing),
               "The following levels in the column 'likeSunak' are not specified in `varLevels`: Strongly dislike.")
  missing <- c("1", "2", "3", "4", "5", "Don't know", "6", "7", "8", "9")
  expect_error(plot_likert(df, vars, varLevels = missing),
               "The following levels in the column 'likeSunak' are not specified in `varLevels`: Strongly dislike, Strongly like.")
  # ==============================================================#
  # checking list type in varLevels
  wrong_names <- list(neg = c("Strongly dislike", "1", "2", "3", "4", "5"),
                      neutral = c("Don't know"),
                      pos = c("6", "7", "8", "9", "Strongly like"))
  expect_error(plot_likert(df, vars, varLevels = wrong_names),
               "`varLevels` must be a named list with 'left', 'neutral', and 'right' elements.")
  misspelt <- list(left = c("Strongly dislike", "1", "2", "3", "4", "5"),
                   neutral = c("Dont know"),
                   right = c("6", "7", "8", "9", "Strongly like"))
  expect_error(plot_likert(df, vars, varLevels = misspelt),
               "The following levels in `varLevels` are not present in the column 'likeSunak': Dont know.")
  additional <- list(left = c("Strongly dislike", "1", "2", "3", "4", "5"),
                     neutral = c("Don't know", "additional_level"),
                     right = c("6", "7", "8", "9", "Strongly like"))
  expect_error(plot_likert(df, vars, varLevels = additional),
               "The following levels in `varLevels` are not present in the column 'likeSunak': additional_level.")
  missing <- list(left = c("Strongly dislike", "1", "2", "3", "4", "5"),
                  neutral = c(),
                  right = c("6", "7", "8", "9", "Strongly like"))
  expect_error(plot_likert(df, vars, varLevels = missing),
               "The following levels in the column 'likeSunak' are not specified in `varLevels`: Don't know.")
  # ==============================================================#
  # checking other arguments
  expect_error(plot_likert(df, vars, varLevels_vector, neutrals = "right"),
               "When `neutrals` is not 'no_change', `NET` is TRUE, or order_by is not NULL, `varLevels` must be a list.")
  expect_error(plot_likert(df, vars, varLevels_vector, neutrals = "exclude"),
               "When `neutrals` is not 'no_change', `NET` is TRUE, or order_by is not NULL, `varLevels` must be a list.")
  expect_error(plot_likert(df, vars, varLevels_vector, order_by = "right"),
               "When `neutrals` is not 'no_change', `NET` is TRUE, or order_by is not NULL, `varLevels` must be a list.")
  expect_warning(plot_likert(df, vars, total = TRUE),
                 "`total` will be ignored because `group` is NULL.")
  expect_warning(plot_likert(df, vars, varLevels_list, type = "stacked", neutrals = "right"),
                 "`neutrals` will be set to 'no_change' as it is only applicable for the 'divergent' type of plot.")
  expect_warning(plot_likert(df, vars, varLevels_list, type = "stacked", neutrals = "exclude"),
                 "`neutrals` will be set to 'no_change' as it is only applicable for the 'divergent' type of plot.")
  # expect_warning(plot_likert(df, vars, varLevels_list, type = "divergent", neutrals = "right", NET = TRUE),
  #                "`NET` will be set to FALSE because `neutrals` is set to 'right'. The function does not permit both.")

  expect_error(plot_likert(df, vars, varLevels_list, order_by = "invalid_entry"),
               "Invalid value for `order_by`. Choose from 'left', 'right', 'NET', or NULL.")
  expect_warning(plot_likert(df, vars, varLevels_list, order_by = "NET"),
                 "`order_by` set to 'NET' will be ignored because `NET` is FALSE. Setting `order_by` to NULL.")
})

# ==============================================================#
# Stacked bar charts
test_that("function return correct plot for stacked bar chart", {
  # weighted, list vars, group = NULL, NET = TRUE
  p <- plot_likert(df, vars, weight = "wt", varLevels = varLevels_list,
                   NET = TRUE, order_by = "left", legend = "bottom")

  expect_equal(p$guides$fill$nrow, 1)
  expect_equal(p$coordinates$clip, "off")
  expect_equal(p$coordinates$ratio, 6)

  # weighted, list vars, group = NULL, NET = FALSE
  p <- plot_likert(df, vars, weight = "wt", varLevels = varLevels_list,
                   legend = "top", labels = TRUE, threshold = 3)

  # unweighted, list vars, group = "gender", NET = TRUE
  p <- plot_likert(df, vars, group = "gender", varLevels = varLevels_list,
                   NET = TRUE, order_by = "NET", legend = "bottom")

  # weighted, list vars, group = "gender", NET = FALSE
  p <- plot_likert(df, vars, group = "gender", weight = "wt", varLevels = varLevels_list,
                   NET = FALSE, order_by = "right", legend = "bottom")

  # unweighted, vector vars, group = "gender", NET = TRUE
  p <- plot_likert(df, vars = "pidWeThey", group = "gender",
                   varLevels = list(left = c("Strongly disagree", "Disagree"),
                                    neutral = "Don't know",
                                    right = c("Agree", "Strongly agree")),
                   colours = colour_pal("divBlueGreen", 5, c("Strongly disagree",
                                                             "Disagree", "Don't know",
                                                             "Agree", "Strongly agree")),
                   NET = TRUE, order_by = "NET", legend = "bottom")

  # weighted, vector vars, group = "gender", NET = FALSE
  p <- plot_likert(df, vars = "pidWeThey", group = "gender", weight = "wt",
                   varLevels = list(left = c("Strongly disagree", "Disagree"),
                                    neutral = "Don't know",
                                    right = c("Agree", "Strongly agree")),
                   NET = FALSE, order_by = "right", legend = "bottom")

  # weighted, vector vars, group = NULL, NET = TRUE
  p <- plot_likert(df, vars = "pidWeThey", group = "gender", weight = "wt",
                   varLevels = list(left = c("Strongly disagree", "Disagree"),
                                    neutral = "Don't know",
                                    right = c("Agree", "Strongly agree")),
                   total = TRUE, NET = TRUE, order_by = "right", legend = "bottom")

  # unweighted, vector vars, group = NULL, NET = FALSE
  p <- plot_likert(df, vars = "pidWeThey",
                   varLevels = list(left = c("Strongly disagree", "Disagree"),
                                    neutral = "Don't know",
                                    right = c("Agree", "Strongly agree")),
                   NET = FALSE, order_by = "left", legend = "bottom")

  # weighted, vector vars, group = NULL, NET = TRUE
  p <- plot_likert(df, vars = "pidWeThey", weight = "wt",
                   varLevels = list(left = c("Strongly disagree", "Disagree"),
                                    neutral = "Don't know",
                                    right = c("Agree", "Strongly agree")),
                   NET = TRUE, order_by = "right", legend = "bottom")

})
