## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## HPDI
hpdi(cal, level = 0.683)
hpdi(cal, level = 0.954)
hpdi(cal, level = 0.997)

## Convert BP scale to CE
CE <- project(cal, "CE")
hpdi(CE, level = 0.954)

## Convert BP scale to b2k
b2k <- project(cal, "b2k")
hpdi(b2k, level = 0.954)
