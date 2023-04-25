## Univariate
z <- series(rnorm(300), era("BCE"), start = 1000, increment = 1)

names(z)
start(z)
end(z)
time(z)

## Multivariate
z <- series(matrix(rnorm(300), 100, 3), era("CE"), start = 1000, increment = 1)

names(z)
start(z)
end(z)
time(z)
