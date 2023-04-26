## Create time-series of 100 observations
## Sampled every years starting from 1000 CE

## Univariate
X <- series(rnorm(300), era("BCE"), start = 1000)

names(X)
start(X)
end(X)
time(X)

## Multivariate
Y <- series(matrix(rnorm(300), 100, 3), era("CE"), start = 1000)

names(Y)
start(Y)
end(Y)
time(Y)
