test_that("Fixed from Gregorian - Date", {
  y <- c(-586, -168, 70, 135, 470)
  m <- c(7, 12, 9, 10, 1)
  d <- c(24, 5, 24, 2, 8)

  z <- as_fixed(y, m, d, calendar = calendar("AD"))
  expect_equal(z@.Data, c(-214193, -61387, 25469, 49217, 171307))

  expect_equal(as_year(z, calendar = calendar("AD")), y)
  expect_equal(as_date(z, calendar = calendar("AD")),
               data.frame(year = y, month = m, day = d))

  AD <- as_fixed(1950, 01, 01, calendar = calendar("AD"))
  BP <- as_fixed(0, 01, 01, calendar = calendar("BP"))
  expect_identical(AD, BP)

  # as_year(711858, calendar = calendar("BC"))
  expect_equal(as_year(AD, calendar = calendar("AD")), 1950)
  expect_equal(as_year(AD, calendar = calendar("BC")), -1950)
  expect_equal(as_year(AD, calendar = calendar("b2k")), 50)
  expect_equal(as_year(AD, calendar = calendar("BP")), 0)

  expect_equal(as_date(AD, calendar = calendar("AD")), data.frame(year = 1950, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = calendar("BC")), data.frame(year = -1950, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = calendar("b2k")), data.frame(year = 50, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = calendar("BP")), data.frame(year = 0, month = 1, day = 1))
})
test_that("Fixed from Gregorian - Fractional year", {
  dec <- as_decimal(2023, 05, 09, calendar = calendar("CE")) # 2023.351

  fix <- as_fixed(dec, calendar = calendar("CE"))
  expect_equal(fix, as_fixed(2023, 05, 09, calendar = calendar("CE")))

  expect_equal(as_date(fix, calendar = calendar("CE")), data.frame(year = 2023, month = 05, day = 09))
})
