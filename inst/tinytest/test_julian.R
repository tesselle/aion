rd <- fixed(
  year = dates$julian_year,
  month = dates$julian_month,
  day = dates$julian_day,
  calendar = J()
)

dec <- as_decimal(
  year = dates$julian_year,
  month = dates$julian_month,
  day = dates$julian_day,
  calendar = J()
)

target <- data.frame(
  year = as.numeric(dates$julian_year),
  month = as.numeric(dates$julian_month),
  day = as.numeric(dates$julian_day)
)

# Rata die <> Julian date ======================================================
expect_equal(rd@.Data, as.numeric(dates$rata_die))
expect_equal(as_year(rd, calendar = J()), target$year)
expect_equal(as_date(rd, calendar = J()), target)

# Shortcuts ====================================================================
expect_equal(fixed_to_julian(fixed_from_AD(01, 01, 01)), 1)
expect_equal(as_date(fixed_from_AD(01, 01, 01), calendar = J()), data.frame(year = 1, month = 1, day = 3))
