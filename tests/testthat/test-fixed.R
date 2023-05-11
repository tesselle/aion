test_that("Fixed from Gregorian - Date", {
  y <- c(-586, -168, 70, 135, 470)
  m <- c(7, 12, 9, 10, 1)
  d <- c(24, 5, 24, 2, 8)

  z <- fixed(y, m, d, calendar = AD())
  expect_equal(z@.Data, c(-214193, -61387, 25469, 49217, 171307))

  expect_equal(as_year(z, calendar = AD()), y)
  expect_equal(as_date(z, calendar = AD()),
               data.frame(year = y, month = m, day = d))

  AD <- fixed(1950, 01, 01, calendar = AD())
  BP <- fixed(0, 01, 01, calendar = BP())
  expect_identical(AD, BP)

  # as_year(711858, calendar = BC())
  expect_equal(as_year(AD, calendar = AD()), 1950)
  expect_equal(as_year(AD, calendar = BC()), -1950)
  expect_equal(as_year(AD, calendar = b2k()), 50)
  expect_equal(as_year(AD, calendar = BP()), 0)

  expect_equal(as_date(AD, calendar = AD()), data.frame(year = 1950, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = BC()), data.frame(year = -1950, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = b2k()), data.frame(year = 50, month = 1, day = 1))
  expect_equal(as_date(AD, calendar = BP()), data.frame(year = 0, month = 1, day = 1))

  ## Leap year
  R <- fixed(year = 2000, month = 02, day = 29, calendar = CE())
  expect_equal(as_date(R, calendar = CE()), data.frame(year = 2000, month = 02, day = 29))
})
test_that("Fixed from Gregorian - Fractional year", {
  dec <- as_decimal(2023, 05, 09, calendar = CE()) # 2023.351

  fix <- fixed(dec, calendar = CE())
  expect_equal(fix, fixed(2023, 05, 09, calendar = CE()))

  expect_equal(as_date(fix, calendar = CE()), data.frame(year = 2023, month = 05, day = 09))
})
