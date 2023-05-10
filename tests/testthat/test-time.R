test_that("Terminal times", {
  y <- as_fixed(1000:1099, calendar = BCE())

  expect_equal(start(y), -401766)
  expect_equal(end(y), -365607)

  x <- series(rnorm(100), time = y)

  expect_identical(start(x), start(y))
  expect_identical(end(x), end(y))
})
test_that("Sampling times", {
  y <- as_fixed(1000:1099, calendar = BCE())

  expect_equal(time(y), y@.Data)

  x <- series(rnorm(100), time = y)

  expect_identical(time(x), sort(time(y)))
})
test_that("Duration", {
  y <- as_fixed(c(1900, 1950), calendar = CE())
  x <- series(rnorm(2), time = y)

  expect_identical(span(x), 18262)
  expect_identical(as_year(span(x), calendar = CE()), 1950-1900)
})
