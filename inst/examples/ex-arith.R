## Vectors of years
x <- as_fixed(c(-350, 31, 1072, 576, 1130), calendar = CE())
y <- as_fixed(c(1494, 1645, -869, 1440, 1851), calendar = CE())

## Move forward in time
x + y

## Move backward in time
x - y
