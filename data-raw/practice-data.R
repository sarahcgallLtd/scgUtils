# the SPSS file has been downloaded from the British Election Study website on 04-Jan-24:
# Wave 25 of the 2014-2023 British Election Study Internet Panel (May 2023 - May 2023) n = 30,407
# https://www.britishelectionstudy.com/data-object/wave-25-of-the-2014-2023-british-election-study-internet-panel/

# read SPSS file and save it in a data frame called "df"
survey <- haven::read_sav("data-raw/other/BES2019_W25_v25.0.sav")

# Limit size to n=5000 and number of variables
survey <- survey[1:5000, c(1, 4:14, 23:35, 59:60, 73:79, 522:525, 527, 531:553, 563, 568:568, 570)]

# save to .rds
usethis::use_data(survey, overwrite = TRUE)

# save to .sav
haven::write_sav(survey, "inst/extdata/survey.sav")

# Unlabel for csv version
survey <- labelled::unlabelled(survey)
# save to .csv
utils::write.table(survey,
                   file = "inst/extdata/survey.csv",
                   na = "",
                   row.names = FALSE,
                   col.names = TRUE,
                   fileEncoding = "UTF-8",
                   sep = ',')

# WEIGHTING PRACTICE SET
# read SPSS file and save it in a data frame called "df"
survey <- haven::read_sav("data-raw/other/BES2019_W25_v25.0.sav")

# Limit size to n=5000 and number of variables
survey_wt <- survey[c("wt","age","ageGroup","gender","p_past_vote_2019","p_turnout_2019")]

# save to .rds
usethis::use_data(survey_wt, overwrite = TRUE)
