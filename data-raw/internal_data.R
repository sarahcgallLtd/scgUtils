# Read csv
colours <- utils::read.csv("data-raw/colours.csv", encoding = "UTF-8")
colours <- dplyr::rename(colours, palette = X.U.FEFF.palette)

# save to .rds
usethis::use_data(colours, internal = TRUE, overwrite = TRUE)


# for any future internal additions, use the following to save to systdata.rda
# sysdata_filenames <- load("R/sysdata.rda")
# save(list = c(sysdata_filenames, "df"), file = "R/sysdata.rda")
