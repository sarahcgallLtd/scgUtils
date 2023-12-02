# Read csv
colours <- utils::read.csv("data-raw/colours.csv", encoding = "UTF-8")
colours <- dplyr::rename(colours, palette = X.U.FEFF.palette)

# save to .rds
usethis::use_data(colours, internal = TRUE, overwrite = TRUE)
