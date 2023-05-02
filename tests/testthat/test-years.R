test_that("Create a vector of years", {
  x <- c(5000, 1000, 2000, -3000)

  y <- years(x, calendar("CE"), sort = FALSE)
  expect_identical(y@.Data, x) # Unordered

  y <- years(x, calendar("CE"), sort = TRUE)
  expect_identical(y@.Data, sort(x)) # Ordered

  CAL <- calendar("CE")
  expect_identical(calendar(y), CAL)
  expect_equal(calendar_label(y), calendar_label(CAL))
  expect_equal(calendar_name(y), calendar_name(CAL))
  expect_equal(calendar_epoch(y), calendar_epoch(CAL))
  expect_equal(calendar_direction(y), calendar_direction(CAL))
  expect_equal(calendar_year(y), calendar_year(CAL))
})
test_that("Extract parts of a vector of years", {
  y <- years(c(5000, 1000, 2000, -3000), calendar("CE"), sort = FALSE)

  z <- y[1:2]
  expect_identical(z@.Data, c(5000, 1000))
  expect_identical(calendar(z), calendar("CE"))

  z <- y[[1]]
  expect_identical(z@.Data, c(5000))
  expect_identical(calendar(z), calendar("CE"))
})
test_that("Replace parts of a vector of years", {
  y <- years(c(5000, 1000, 2000, -3000), calendar("CE"), sort = FALSE)

  y[c(2, 4)] <- c(1, 3)
  expect_identical(y@.Data, c(5000, 1, 2000, 3))
  expect_identical(calendar(y), calendar("CE"))

  y[[1]] <- -1
  expect_identical(y@.Data, c(-1, 1, 2000, 3))
  expect_identical(calendar(y), calendar("CE"))
})
