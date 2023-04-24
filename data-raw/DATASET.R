## code to prepare `DATASET` dataset goes here
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
    name = "years before 2000",
    epoch = 2000,
    scale = 1L,
    direction = -1L,
    unit = "gregorian"
  )
)
usethis::use_data(.unit, .era, internal = TRUE, overwrite = TRUE)
