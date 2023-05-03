test_that("Create a vector of years", {
  x <- c(5000, 1000, 2000, -3000)

  y <- years(x, era("CE"), sort = FALSE)
  expect_identical(y@.Data, x) # Unordered

  y <- years(x, era("CE"), sort = TRUE)
  expect_identical(y@.Data, sort(x)) # Ordered

  CAL <- era("CE")
  expect_identical(era(y), CAL)
  expect_equal(era_label(y), era_label(CAL))
  expect_equal(era_name(y), era_name(CAL))
  expect_equal(era_epoch(y), era_epoch(CAL))
  expect_equal(era_direction(y), era_direction(CAL))
  expect_equal(era_year(y), era_year(CAL))
})
test_that("Extract parts of a vector of years", {
  y <- years(c(5000, 1000, 2000, -3000), era("CE"), sort = FALSE)

  z <- y[1:2]
  expect_identical(z@.Data, c(5000, 1000))
  expect_identical(era(z), era("CE"))

  z <- y[[1]]
  expect_identical(z, c(5000))
  expect_s4_class(z, NA)
})
test_that("Replace parts of a vector of years", {
  y <- years(c(5000, 1000, 2000, -3000), era("CE"), sort = FALSE)

  y[c(2, 4)] <- c(1, 3)
  expect_identical(y@.Data, c(5000, 1, 2000, 3))
  expect_identical(era(y), era("CE"))

  y[[1]] <- -1
  expect_identical(y@.Data, c(-1, 1, 2000, 3))
  expect_identical(era(y), era("CE"))
})
