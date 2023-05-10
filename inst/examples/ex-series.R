## Create time-series of 100 observations

## Univariate
## Sampled every years starting from 1099 BCE
(X <- series(rnorm(100), time = 1099:1000, calendar = BCE()))

start(X)
end(X)
span(X)
time(X)

## Multivariate
## Sampled every century starting from 1000 CE
(Y <- series(matrix(rnorm(300), 100, 3), time = 1000:1099, calendar = CE()))

start(Y)
end(Y)
span(Y)
time(Y)
