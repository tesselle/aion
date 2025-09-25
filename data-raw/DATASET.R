# External data ================================================================
dates <- read.table("data-raw/reingold.csv", header = TRUE, sep = ",", dec = ".")
usethis::use_data(dates, overwrite = TRUE)
