## Create time series
mtx <- matrix(rnorm(300), 100, 3)
(x <- series(mtx, era("CE"), start = 1000, increment = 1))

## Subset
(y <- window(x, start = 1025, end = 1050))
