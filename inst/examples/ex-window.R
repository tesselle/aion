## Create time series
(x <- series(matrix(rnorm(300), 100, 3), era("CE"), start = 1000))

## Subset
(y <- window(x, start = 1025, end = 1050))
