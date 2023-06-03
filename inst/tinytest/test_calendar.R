# Unknown calendar =============================================================
expect_error(calendar("XXX"), "Unknown calendar")

# Gregorian calendar ===========================================================
G <- calendar("BP")

expect_true(is_gregorian(G))
expect_false(is_julian(G))

expect_identical(format(G), "Gregorian years BP")
expect_identical(calendar_label(G), "BP")
expect_identical(calendar_name(G), "Before Present")
expect_identical(calendar_epoch(G), 1950)
expect_identical(calendar_direction(G), -1)
expect_identical(aion:::calendar_fixed(G), 1) # Inherited from GregorianCalendar

# Julian calendar ==============================================================
J <- calendar("julian")

expect_true(is_julian(J))
expect_false(is_gregorian(J))

expect_identical(format(J), "Julian years")
expect_identical(calendar_label(J), character(0))
expect_identical(calendar_name(J), character(0))
expect_identical(calendar_epoch(J), 1)
expect_identical(calendar_direction(J), 1)
expect_identical(aion:::calendar_fixed(J), -1)
