## Create 6 time-series of 50 observations
## Sampled every two years starting from 2000 BP
X <- series(
  data = matrix(rnorm(300), nrow = 50, ncol = 6),
  scale = era("BP"),
  start = 2000,
  delta = 2
)

## Plot
plot(X)
