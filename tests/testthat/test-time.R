test_that("Terminal times", {
  y <- as_fixed(1000:1099, calendar = calendar("BCE"))

  expect_equal(start(y), -401766)
  expect_equal(end(y), -365607)

  x <- series(rnorm(100), time = y)

  expect_identical(start(x), start(y))
  expect_identical(end(x), end(y))
})
test_that("Sampling times", {
  y <- as_fixed(1000:1099, calendar = calendar("BCE"))

  expect_equal(time(y), y@.Data)

  x <- series(rnorm(100), time = y)

  expect_identical(time(x), sort(time(y)))
})
