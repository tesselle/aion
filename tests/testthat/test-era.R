test_that("Era", {
  expect_error(era("XXX"), "Unknown era")
})
test_that("BP", {
  cal <- era("BP")

  expect_true(is_gregorian(cal))
  expect_false(is_julian(cal))

  expect_equal(era_label(cal), "BP")
  expect_equal(era_name(cal), "Before Present")
  expect_equal(era_epoch(cal), 1950)
  expect_equal(era_direction(cal), -1L)
  expect_equal(era_year(cal), 365.2425) # Inherited from GregorianCalendar
})
