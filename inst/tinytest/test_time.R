# Terminal times ===============================================================
y <- fixed(1000:1099, calendar = BCE())
x <- series(rnorm(100), time = y)

expect_identical(start(x), min(y))
expect_identical(start(x, calendar = CE()), -1099)
expect_identical(end(x), max(y))
expect_identical(end(x, calendar = CE()), -1000)

# Sampling times ===============================================================
y <- fixed(seq(1000, 1090, by = 2), calendar = BCE())
x <- series(rnorm(46), time = y)

expect_identical(as.numeric(x@.Time), sort(y@.Data))
expect_identical(time(x, calendar = CE()), sort(as_year(y, calendar = CE())))
expect_equal(round(frequency(x), 6), 0.001369)

# Duration ===============================================================
y <- fixed(c(1900, 1950), calendar = CE())
x <- series(rnorm(2), time = y)

expect_identical(span(x), 18262)
expect_identical(span(x, calendar = CE()), 1950 - 1900)
expect_identical(span(x, calendar = CE()), span(x, calendar = BP()))
