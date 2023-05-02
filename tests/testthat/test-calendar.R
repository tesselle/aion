test_that("Initialize a calendar", {
  expect_error(calendar("XXX"), "Unknown calendar")

  ## Create a custom gregorian calendar
  cal <- as_gregorian(
    label = "cal BP",
    name = "Before Present",
    epoch = 1950,
    direction = -1
  )

  expect_equal(calendar_label(cal), "cal BP")
  expect_equal(calendar_name(cal), "Before Present")
  expect_equal(calendar_epoch(cal), 1950)
  expect_equal(calendar_direction(cal), -1L)
  expect_equal(calendar_year(cal), 365.2425) # Inherited from GregorianCalendar
})
test_that("BP", {
  cal <- calendar("BP")

  expect_equal(calendar_label(cal), "BP")
  expect_equal(calendar_name(cal), "Before Present")
  expect_equal(calendar_epoch(cal), 1950)
  expect_equal(calendar_direction(cal), -1L)
  expect_equal(calendar_year(cal), 365.2425) # Inherited from GregorianCalendar
})
