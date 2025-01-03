Sys.setenv(LANGUAGE = "en") # Force locale

rd <- fixed(
  year = dates$gregorian_year,
  month = dates$gregorian_month,
  day = dates$gregorian_day,
  calendar = CE()
)

dec <- as_decimal(
  year = dates$gregorian_year,
  month = dates$gregorian_month,
  day = dates$gregorian_day,
  calendar = CE()
)

target <- data.frame(
  year = as.numeric(dates$gregorian_year),
  month = as.numeric(dates$gregorian_month),
  day = as.numeric(dates$gregorian_day)
)

# Rata die <> Gregorian date ===================================================
expect_equal(rd@.Data, as.numeric(dates$rata_die))
expect_equal(as_year(rd, calendar = CE(), decimal = FALSE), target$year)
expect_equal(as_year(rd, calendar = CE(), decimal = TRUE), dec)
expect_equal(as_date(rd, calendar = CE()), target)

# Rata die <> Gregorian decimal year ===========================================
expect_equal(round(as_decimal(2023, 05, 09, calendar = CE()), 9), 2023.350684936)
expect_equal(round(as_decimal(2000, 02, 29, calendar = CE()), 9), 2000.161202186)

# WHY ???
fix <- fixed(dec, calendar = CE())
expect_equal(fix@.Data, as.numeric(dates$rata_die))
expect_equal(as_date(fix, calendar = CE()), target)
expect_equal(as_year(fix, calendar = CE(), decimal = FALSE), target$year)
expect_equal(as_year(fix, calendar = CE(), decimal = TRUE), dec)

# Shortcuts ====================================================================
expect_equal(fixed_to_AD(fixed_from_AD(1950, 01, 01)), 1950)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = AD()), data.frame(year = 1950, month = 1, day = 1))

expect_equal(fixed_to_CE(fixed_from_AD(1950, 01, 01)), 1950)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = CE()), data.frame(year = 1950, month = 1, day = 1))

expect_equal(fixed_to_BC(fixed_from_AD(1950, 01, 01)), -1950)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = BC()), data.frame(year = -1950, month = 1, day = 1))

expect_equal(fixed_to_BCE(fixed_from_AD(1950, 01, 01)), -1950)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = BCE()), data.frame(year = -1950, month = 1, day = 1))

expect_equal(fixed_to_BCE(fixed_from_AD(1950, 01, 01)), -1950)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = BCE()), data.frame(year = -1950, month = 1, day = 1))

expect_equal(fixed_to_b2k(fixed_from_AD(1950, 01, 01)), 50)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = b2k()), data.frame(year = 50, month = 1, day = 1))

expect_equal(fixed_to_BP(fixed_from_AD(1950, 01, 01)), 0)
expect_equal(as_date(fixed_from_AD(1950, 01, 01), calendar = BP()), data.frame(year = 0, month = 1, day = 1))
