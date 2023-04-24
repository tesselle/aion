# External data ================================================================
ksarakil <- read.table(file = "data-raw/ksarakil.csv", header = TRUE,
                       sep = ",", dec = ".")
usethis::use_data(ksarakil, overwrite = TRUE)

# Internal data ================================================================
.unit <- list(
  gregorian = list(unit = "Gregorian", length = 365.2425),
  julian = list(unit = "Julian", length = 365.25)
)
.era <- list(
  BP = list(
    label = "BP",
    name = "Before Present",
    epoch = 1950,
    scale = 1L,
    direction = -1L,
    unit = "gregorian"
  ),
  BC = list(
    label = "BC",
    name = "Before Christ",
    epoch = 1,
    scale = 1L,
    direction = -1L,
    unit = "gregorian"
  ),
  BCE = list(
    label = "BCE",
    name = "Before Common Era",
    epoch = 1,
    scale = 1L,
    direction = -1L,
    unit = "gregorian"
  ),
  AD = list(
    label = "AD",
    name = "Anno Domini",
    epoch = 0,
    scale = 1L,
    direction = 1L,
    unit = "gregorian"
  ),
  CE = list(
    label = "CE",
    name = "Common Era",
    epoch = 0,
    scale = 1L,
    direction = 1L,
    unit = "gregorian"
  ),
  b2k = list(
    label = "b2k",
    name = "Before 2000",
    epoch = 2000,
    scale = 1L,
    direction = -1L,
    unit = "gregorian"
  )
)

## Allen codes
.allen_code <- c("<", "m", "o", "fi", "di", "s", "=", "si", "d", "f", "oi", "mi", ">")
.allen_converse <- c(">", "mi", "oi", "f", "d", "si", "=", "s", "di", "fi", "o", "m", "<")
.allen_string <- c(
  `<` = "precedes",     `m` = "meets",          `o` = "overlaps",
  `fi` = "finished by", `di` = "contains",      `s` = "starts",
  `=` = "equals",       `si` = "started by",    `d` = "during",
  `f` = "finishes",     `oi` = "overlapped by", `mi` = "met by",
  `>` = "preceded by"
)

## Alspaugh codes
.alspaugh_code <- c("p", "m", "o", "F", "D", "s", "e", "S", "d", "f", "O", "M", "P")
.alspaugh_converse <- c("P", "M", "O", "f", "d", "S", "e", "s", "D", "F", "o", "m", "p")
.alspaugh_string <- c(
  `p` = "precedes",    `m` = "meets",         `o` = "overlaps",
  `F` = "finished by", `D` = "contains",      `s` = "starts",
  `e` = "equals",      `S` = "started by",    `d` = "during",
  `f` = "finishes",    `O` = "overlapped by", `M` = "met by",
  `P` = "preceded by"
)

## Table of composition of basic Allen interval relations
.composition <- read.table("data-raw/allen.csv", header = TRUE, sep = ",", row.names = 1)

usethis::use_data(
  .unit, .era,
  .allen_code, .allen_string, .allen_converse, .composition,
  .alspaugh_code, .alspaugh_converse, .alspaugh_string,
  internal = TRUE,
  overwrite = TRUE
)
