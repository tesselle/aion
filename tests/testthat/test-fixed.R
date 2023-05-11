test_that("Fixed from Gregorian - Date", {
  y <- c(-586, -168, 70, 135, 470)
  m <- c(7, 12, 9, 10, 1)
  d <- c(24, 5, 24, 2, 8)

  z <- fixed(y, m, d, calendar = CE())
  expect_equal(z@.Data, c(-214193, -61387, 25469, 49217, 171307))

  expect_equal(as_year(z, calendar = CE(), decimal = FALSE), y)
  expect_equal(as_date(z, calendar = CE()),
               data.frame(year = y, month = m, day = d))

  ## Standard year
  CE <- fixed(1950, 01, 01, calendar = CE())
  BP <- fixed(0, 01, 01, calendar = BP())
  expect_identical(CE, BP)

  expect_equal(as_date(CE, calendar = AD()), data.frame(year = 1950, month = 1, day = 1))
  expect_equal(as_date(CE, calendar = CE()), data.frame(year = 1950, month = 1, day = 1))
  expect_equal(as_date(CE, calendar = BC()), data.frame(year = -1950, month = 1, day = 1))
  expect_equal(as_date(CE, calendar = BCE()), data.frame(year = -1950, month = 1, day = 1))
  expect_equal(as_date(CE, calendar = b2k()), data.frame(year = 50, month = 1, day = 1))
  expect_equal(as_date(CE, calendar = BP()), data.frame(year = 0, month = 1, day = 1))

  ## Leap year
  R <- fixed(2000, 02, 29, calendar = CE())
  expect_equal(as_date(R, calendar = CE()), data.frame(year = 2000, month = 02, day = 29))
})
test_that("Fixed from Gregorian - Fractional year", {
  dec <- as_decimal(2023, 05, 09, calendar = CE()) # 2023.351

  fix <- fixed(dec, calendar = CE())
  expect_equal(fix, fixed(2023, 05, 09, calendar = CE()))
  expect_equal(as_date(fix, calendar = CE()), data.frame(year = 2023, month = 05, day = 09))

  expect_equal(as_year(fix, calendar = CE(), decimal = TRUE), dec)
})
