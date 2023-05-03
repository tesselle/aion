test_that("Gregorian calendar", {
  ## Create a custom gregorian calendar
  cal <- as_gregorian(
    label = "AUC",
    name = "Ab urbe condita",
    epoch = 753,
    direction = 1
  )

  expect_equal(era_label(cal), "AUC")
  expect_equal(era_name(cal), "Ab urbe condita")
  expect_equal(era_epoch(cal), 753)
  expect_equal(era_direction(cal), 1L)
  expect_equal(era_year(cal), 365.2425) # Inherited from GregorianCalendar
})
