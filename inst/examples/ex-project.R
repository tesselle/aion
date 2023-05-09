## Create time series
A <- series(rnorm(100), calendar("CE"), time = 1000:1099)
B <- series(rnorm(100), calendar("CE"), time = 1100:1199)

## Reproject to BP time scale
(A <- project(A, calendar("BP")))
(B <- project(B, A))
