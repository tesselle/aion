Sys.setenv(LANGUAGE = "en") # Force locale

# Default calendar =============================================================
expect_identical(get_calendar(), CE())
expect_identical(set_calendar("BCE"), BCE())
expect_identical(get_calendar(), BCE())
expect_identical(set_calendar(), CE()) # Reset

options("aion.calendar" = BCE())
expect_error(get_calendar())
options("aion.calendar" = NULL)

# Unknown calendar =============================================================
expect_error(calendar("XXX"), "Unknown calendar")

# Gregorian calendar ===========================================================
G <- calendar("BP")

expect_true(is_calendar(G))
expect_true(is_gregorian(G))
expect_false(is_julian(G))

expect_identical(format(G), "Gregorian years BP")
expect_identical(calendar_label(G), "BP")
expect_identical(calendar_name(G), "Before Present")
expect_identical(calendar_epoch(G), 1950)
expect_identical(calendar_direction(G), -1)
expect_identical(calendar_fixed(G), 1)
expect_identical(calendar_year(G), 365.2425)

# Julian calendar ==============================================================
J <- calendar("julian")

expect_true(is_calendar(J))
expect_true(is_julian(J))
expect_false(is_gregorian(J))

expect_identical(format(J), "Julian years")
expect_identical(calendar_label(J), "")
expect_identical(calendar_name(J), "")
expect_identical(calendar_epoch(J), 1)
expect_identical(calendar_direction(J), 1)
expect_identical(calendar_fixed(J), -1)
expect_identical(calendar_year(J), 365.25)
