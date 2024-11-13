Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(aion.calendar = calendar("CE"))

# Format =======================================================================
expect_identical(format(CE()), "Gregorian years CE")
expect_identical(format(J()), "Julian years")

x <- fixed(c(30, 35, 40), calendar = calendar("BP"), scale = 1000)

expect_identical(format(x), c("-28050 CE", "-33050 CE", "-38050 CE"))
expect_identical(format(x, prefix = "ka"), c("-28.05 ka CE", "-33.05 ka CE", "-38.05 ka CE"))
expect_identical(format(x, prefix = "Ma"), c("-0.02805 Ma CE", "-0.03305 Ma CE", "-0.03805 Ma CE"))
expect_identical(format(x, prefix = "Ga"), c("-2.805e-05 Ga CE", "-3.305e-05 Ga CE", "-3.805e-05 Ga CE"))
expect_identical(format(x, prefix = TRUE), c("-28.05 ka CE", "-33.05 ka CE", "-38.05 ka CE"))

# Show =========================================================================
if (at_home()) {
  using("tinysnapshot")

  # TimeScale
  expect_snapshot_print(CE(), label = "show_calendar_CE")
  expect_snapshot_print(J(), label = "show_calendar_julian")

  # RataDie
  x <- fixed(c(-350, 31, 1072, 576, 1130), calendar = CE())
  expect_snapshot_print(x, label = "show_rata_die")

  # TimeSeries
  y <- series(matrix(rnorm(300), 100, 3), time = 1000:1099, calendar = CE())
  expect_snapshot_print(y, label = "show_time_series")
}
