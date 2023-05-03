test_that("Create from vector", {
  x <- rnorm(100)

  X <- series(x, era("BCE"), time = 1000:1099)
  Y <- series(x, years(1000:1099, era("BCE")))
  expect_identical(X, Y)

  CAL <- era("BCE")
  expect_identical(era(X), CAL)
  expect_equal(era_label(X), era_label(CAL))
  expect_equal(era_name(X), era_name(CAL))
  expect_equal(era_epoch(X), era_epoch(CAL))
  expect_equal(era_direction(X), era_direction(CAL))
  expect_equal(era_year(X), era_year(CAL))
})
test_that("Create from matrix", {
  x <- matrix(rnorm(300), 100, 3)

  X <- series(x, era("BCE"), time = 1000:1099, names = c("A", "B", "C"))
  Y <- series(x, years(1000:1099, era("BCE")), names = c("A", "B", "C"))
  expect_identical(X, Y)
})
test_that("Create from data.frame", {
  x <- as.data.frame(matrix(rnorm(300), 100, 3))

  X <- series(x, era("BCE"), time = 1000:1099)
  Y <- series(x, years(1000:1099, era("BCE")))
  expect_identical(X, Y)
})
