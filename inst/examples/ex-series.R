## Create time-series of 20 observations

## Univariate
## Sampled every years starting from 1029 BCE
(X <- series(rnorm(30), time = 1029:1000, calendar = BCE()))

start(X)
end(X)
time(X)

## Multivariate
## Sampled every century starting from 1000 CE
(Y <- series(matrix(rnorm(90), 30, 3), time = 1000:1029, calendar = CE()))

start(Y, calendar = CE())
end(Y, calendar = CE())
span(Y, calendar = CE())
time(Y, calendar = CE())
