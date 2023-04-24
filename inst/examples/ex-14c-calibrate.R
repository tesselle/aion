## Calibrate a single date
cal <- c14_calibrate(300, 20)
plot(cal)

## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, panel.first = graphics::grid())

plot(cal, density = TRUE, interval = FALSE)
plot(cal, density = FALSE, interval = TRUE)

## Convert BP scale to CE
CE <- project(cal, "CE")
plot(CE)

## Convert BP scale to b2k
b2k <- project(cal, "b2k")
plot(b2k)
