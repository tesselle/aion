test_that("Terminal times", {
  y <- years(1000:1099, era("BCE"))

  expect_equal(start(y), 1000)
  expect_equal(end(y), 1099)

  x <- series(rnorm(100), time = y)

  expect_identical(start(x), start(y))
  expect_identical(end(x), end(y))
})
test_that("Sampling times", {
  y <- years(1000:1099, era("BCE"))

  expect_equal(time(y), 1000:1099)

  x <- series(rnorm(100), time = y)

  expect_identical(time(x), time(y))
})
