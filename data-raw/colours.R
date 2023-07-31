# LIBRARIES
library("tidyverse")

# Read csv
colours <- read.csv("data-raw/colours.csv", encoding = "UTF-8")
colours <- rename(colours, palette = X.U.FEFF.palette)

# save to .rds
usethis::use_data(colours, internal = TRUE, overwrite = TRUE)
