test_that("Create from vector", {
  x <- rnorm(100)

  X <- series(x, calendar("BCE"), time = 1000:1099)
  Y <- series(x, years(1000:1099, calendar("BCE")))
  expect_identical(X, Y)

  CAL <- calendar("BCE")
  expect_identical(calendar(X), CAL)
  expect_equal(calendar_label(X), calendar_label(CAL))
  expect_equal(calendar_name(X), calendar_name(CAL))
  expect_equal(calendar_epoch(X), calendar_epoch(CAL))
  expect_equal(calendar_direction(X), calendar_direction(CAL))
  expect_equal(calendar_year(X), calendar_year(CAL))
})
test_that("Create from matrix", {
  x <- matrix(rnorm(300), 100, 3)

  X <- series(x, calendar("BCE"), time = 1000:1099, names = c("A", "B", "C"))
  Y <- series(x, years(1000:1099, calendar("BCE")), names = c("A", "B", "C"))
  expect_identical(X, Y)
})
test_that("Create from data.frame", {
  x <- as.data.frame(matrix(rnorm(300), 100, 3))

  X <- series(x, calendar("BCE"), time = 1000:1099)
  Y <- series(x, years(1000:1099, calendar("BCE")))
  expect_identical(X, Y)
})
