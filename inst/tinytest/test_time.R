# Terminal times ===============================================================
y <- fixed(1000:1099, calendar = BCE())
x <- series(rnorm(100), time = y)

expect_identical(start(x), min(y))
expect_identical(end(x), max(y))

# Sampling times ===============================================================
y <- fixed(seq(1000, 1090, by = 2), calendar = BCE())
x <- series(rnorm(46), time = y)

expect_identical(time(x), sort(y@.Data))
expect_identical(years(x, calendar = CE()), sort(as_year(y, calendar = CE())))
expect_equal(round(frequency(x), 6), 0.001369)

# Duration ===============================================================
y <- fixed(c(1900, 1950), calendar = CE())
x <- series(rnorm(2), time = y)

expect_identical(span(x), 18262)
expect_identical(as_year(span(x), calendar = CE(), decimal = FALSE), 1950-1900)
