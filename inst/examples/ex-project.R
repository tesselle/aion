## Create time series
A <- series(rnorm(100), era("CE"), time = 1000:1099)
B <- series(rnorm(100), era("CE"), time = 1100:1199)

## Reproject to BP time scale
(A <- project(A, era("BP")))
(B <- project(B, A))
