test_that("Rata die <> Julian date", {
  y <- c(-587, -169, 70, 135, 470)
  m <- c(7, 12, 9, 10, 1)
  d <- c(30, 8, 26, 3, 7)

  z <- fixed(y, m, d, calendar = J())
  expect_equal(z@.Data, c(-214193, -61387, 25469, 49217, 171307))

  expect_equal(as_year(z, calendar = J()), y)
  expect_equal(as_date(z, calendar = J()),
               data.frame(year = y, month = m, day = d))
})
