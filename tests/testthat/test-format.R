test_that("BP", {
  x <- fixed(c(30, 35, 40), calendar = calendar("BP"), scale = 1000)

  expect_snapshot(format(x))
  expect_snapshot(format(x, format = "ka"))
  expect_snapshot(format(x, format = "Ma"))
  expect_snapshot(format(x, format = "Ga"))
  expect_snapshot(format(x, format = TRUE))
})
