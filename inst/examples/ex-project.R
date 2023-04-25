## Create time series
A <- series(rnorm(300), scale = era("CE"), start = 1000, increment = 1)
B <- series(rnorm(300), scale = era("CE"), start = 1000, increment = 1)

## Reproject to BP time scale
(A <- project(A, "BP"))
(B <- project(B, A))
