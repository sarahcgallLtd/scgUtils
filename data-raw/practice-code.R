library(scgElectionsNZ)
library(dplyr)
df <- get_data("split_total")
df <- df %>%
  filter(Election == 2023) %>%
  mutate(List_Party = ifelse(
    List_Party %in% c("Labour Party", "ACT Party", "Maori Party",
                      "Green Party", "National Party", "NZ First",
                      "Informal"),
    List_Party, "Other"
  )) %>%
  mutate(Electorate_Party = ifelse(
    Electorate_Party %in%
      c("Labour Party", "ACT Party", "Maori Party", "Green Party",
        "National Party", "NZ First", "Informal"),
    Electorate_Party, "Other" # NB includes "Party Vote Only"
  )) %>%
  group_by(List_Party, Electorate_Party) %>%
  summarise(Vote = sum(Votes)) %>%
  ungroup()

plot_sankey(
  data = df,
  source = "Electorate_Party", # left side of sankey
  target = "List_Party", # right side of sankey
  value = "Vote",
  units = '" votes"',
  colours = '"#ffd006","#45ba52","#d5cdb9","#D82A20","#B2001A","#000000","#00529F","#cdcdd1"'
) %>%
  # save from viewer to html
  htmlwidgets::saveWidget(file = "data-raw/sankey_2023.html", selfcontained = TRUE)


### Survey data
# save from viewer to html
df <- get_data("survey")
df <- labelled::unlabelled(df)

plot_popn(data=df,
          xVar="gender",
          yVar="ageGroup",
          weight="wt",
          #meanVar="age",
          #group="partyId",
          #addLabels = "yes",
          #thresholdLab = 5
)
# check removed

# =================================================================================================================== #
# Testing the grid_vars code:
df <- get_data("survey")
df <- labelled::unlabelled(df)
vars <- list(likeSunak = "Sunak",
             likeStarmer = "Starmer",
             likeCon = "Conservatives",
             likeLab = "Labour",
             likeLD = "Lib Dems",
             likeSNP = "SNP",
             likePC = "Plaid",
             likeBrexitParty = "Brexit Party",
             likeGrn = "Greens"
)



grid_vars(data=df,
          vars=vars, weight="wt", group="gender"
)

# Testing the grp_freq code:
grp_freq(df, groups=c("gender"), add_percent = "yes")
