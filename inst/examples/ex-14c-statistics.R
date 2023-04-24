## Calibrate a single date
cal <- c14_calibrate(130, 20)

## Statistics
median(cal)
mean(cal)

## Plot
plot(cal, panel.first = graphics::grid())
abline(v = median(cal), lty = 2, col = "blue")
abline(v = mean(cal), lty = 2, col = "red")
