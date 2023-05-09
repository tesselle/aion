test_that("Gregorian era", {
  expect_error(calendar("XXX"), "Unknown calendar")
})
test_that("BP", {
  cal <- calendar("BP")

  expect_true(is_gregorian(cal))
  expect_false(is_julian(cal))

  expect_equal(calendar_label(cal), "BP")
  expect_equal(calendar_name(cal), "Before Present")
  expect_equal(calendar_epoch(cal), 1950)
  expect_equal(calendar_direction(cal), -1L)
  expect_equal(calendar_year(cal), 365.2425) # Inherited from GregorianCalendar
})
test_that("Gregorian calendar", {
  ## Create a custom gregorian calendar
  cal <- as_gregorian(
    label = "AUC",
    name = "Ab Urbe Condita",
    epoch = 753,
    direction = 1
  )

  expect_true(is_gregorian(cal))

  expect_equal(calendar_label(cal), "AUC")
  expect_equal(calendar_name(cal), "Ab Urbe Condita")
  expect_equal(calendar_epoch(cal), 753)
  expect_equal(calendar_direction(cal), 1L)
  expect_equal(calendar_year(cal), 365.2425) # Inherited from GregorianCalendar
})
