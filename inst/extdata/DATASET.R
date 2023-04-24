col_names <- c("calBP", "age", "error", "delta", "sigma")

# IntCal =======================================================================
intcal20 <- read.table(file = "data-raw/intcal20.14c", header = TRUE,
                       sep = ",", dec = ".", comment.char = "#")
colnames(intcal20) <- col_names
usethis::use_data(intcal20, overwrite = TRUE, compress = "bzip2", version = 3)
