test_that("Calendar", {
  expect_error(calendar("XXX"), "Unknown calendar")
})
test_that("Gregorian calendar", {
  cal <- calendar("BP")

  expect_snapshot(format(cal))

  expect_true(is_gregorian(cal))
  expect_false(is_julian(cal))

  expect_equal(calendar_label(cal), "BP")
  expect_equal(calendar_name(cal), "Before Present")
  expect_equal(calendar_epoch(cal), 1950)
  expect_equal(calendar_direction(cal), -1L)
  # expect_equal(calendar_year(cal), 365.2425) # Inherited from GregorianCalendar
})
