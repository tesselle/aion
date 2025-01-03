Sys.setenv(LANGUAGE = "en") # Force locale

x <- series(matrix(rnorm(300), 100, 3), time = 1000:1099, calendar = CE())

## Subset between 1025 and 1050 CE
y1 <- window(x, start = 374009, end = 383140)
expect_identical(start(y1), 374009)
expect_identical(end(y1), 383140)
expect_identical(dim(y1), c(26L, 3L, 1L))

y2 <- window(x, start = NULL, end = 383140)
expect_identical(start(y2), start(x))
expect_identical(end(y2), 383140)

y3 <- window(x, start = 374009, end = NULL)
expect_identical(start(y3), 374009)
expect_identical(end(y3), end(x))
