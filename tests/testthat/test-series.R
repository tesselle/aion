test_that("Create from vector", {
  x <- rnorm(91)

  X <- series(x, time = seq(100, 109, 0.1), calendar = calendar("BCE"), scale = 10)
  Y <- series(x, fixed(1000:1090, calendar = calendar("BCE")))
  expect_identical(X, Y)
})
test_that("Create from matrix", {
  x <- matrix(rnorm(300), 100, 3)

  X <- series(x, time = 1000:1099, calendar = calendar("BCE"), names = c("A", "B", "C"))
  Y <- series(x, time = fixed(1000:1099, calendar = calendar("BCE")), names = c("A", "B", "C"))
  expect_identical(X, Y)
})
test_that("Create from data.frame", {
  x <- as.data.frame(matrix(rnorm(300), 100, 3))

  X <- series(x, time = 1000:1099, calendar = calendar("BCE"), scale = 10)
  Y <- series(x, time = fixed(1000:1099, calendar = calendar("BCE"), scale = 10))
  expect_identical(X, Y)
})
test_that("Chronological order", {
  x <- matrix(rnorm(300), 100, 3)
  rd <- fixed(year = 1000:1099, calendar = calendar("BCE"))

  X <- series(x, time = rd)

  i <- sample(100)
  Y <- series(x[i, ], time = rd[i]) # Reorder
  expect_identical(X, Y)

  expect_warning(series(x, time = rd, calendar = CE()), "expressed in rata die")
})
