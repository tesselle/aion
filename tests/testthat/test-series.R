test_that("Create from vector", {
  x <- rnorm(91)

  X <- series(x, time = seq(100, 109, 0.1), calendar = calendar("BCE"), scale = 10)
  Y <- series(x, as_fixed(1000:1090, calendar = calendar("BCE")))
  expect_identical(X, Y)
})
test_that("Create from matrix", {
  x <- matrix(rnorm(300), 100, 3)

  X <- series(x, time = 1000:1099, calendar = calendar("BCE"), names = c("A", "B", "C"))
  Y <- series(x, time = as_fixed(1000:1099, calendar = calendar("BCE")), names = c("A", "B", "C"))
  expect_identical(X, Y)
})
test_that("Create from data.frame", {
  x <- as.data.frame(matrix(rnorm(300), 100, 3))

  X <- series(x, time = 1000:1099, calendar = calendar("BCE"), scale = 10)
  Y <- series(x, time = as_fixed(1000:1099, calendar = calendar("BCE"), scale = 10))
  expect_identical(X, Y)
})
