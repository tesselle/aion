## Create 3 time-series of 100 observations
## Sampled every years starting from 1000 CE
(x <- series(matrix(rnorm(300), 100, 3), calendar("CE"), time = 1000:1099))

## Subset between 1025 and 1050 CE
(y <- window(x, start = 1025, end = 1050))
