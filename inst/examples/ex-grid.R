## Create a time-series of 300 observations
## Sampled every two years starting from 2000 BP
X <- series(
  object = rnorm(300),
  time = seq(2000, by = 2, length.out = 300),
  calendar = BP()
)

## Axis
plot(X, axes = FALSE, calendar = BP()) # Remove axes
year_axis(X, side = 1) # Same calendar as last plot
year_axis(X, side = 3, calendar = CE()) # Specific calendar
mtext(format(CE()), side = 3, line = 3)

## Grid
## grid() does not align with the tick marks of the default axis
## because x coordinates of the plotting region are expressed in rata die
## but year_axis() computes tickmarks according to a given calendar
plot(X, panel.first = graphics::grid())

plot(X, panel.first = year_grid(12))
plot(X, panel.first = year_grid(12, NA))
