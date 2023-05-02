## Create 6 time-series of 50 observations
## Sampled every two years starting from 2000 BP
X <- series(
  object = matrix(rnorm(300), nrow = 50, ncol = 6),
  time = seq(2000, by = 2, length.out = 50),
  calendar = calendar("BP")
)

## Plot
plot(X)
