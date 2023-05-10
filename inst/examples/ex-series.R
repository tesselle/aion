## Create time-series of 100 observations

## Univariate
## Sampled every years starting from 1000 CE
(X <- series(rnorm(100), calendar("BCE"), time = 1000:1099))

start(X)
end(X)
time(X)

## Multivariate
## Sampled every century starting from 1000 CE
(Y <- series(matrix(rnorm(300), 100, 3), calendar("CE"), time = 1000:1099))

start(Y)
end(Y)
time(Y)
