## Vectors of years
x <- fixed(c(-350, 31, 1072, 576, 1130), calendar = CE())
y <- fixed(c(1494, 1645, -869, 1440, 1851), calendar = CE())

# Rata die vs Rata die =========================================================
expect_identical(x + y, chronos:::.RataDie(c(417109, 611418, 73414, 735599, 1088058)))
expect_identical(x - y, chronos:::.RataDie(c(-673507, -589502, 708936, -315569, -263340)))

## Not rata die anymore
expect_identical(x * y, c(-69907940292, 6579840680, -124300159175, 110380523760, 278630563941))

# Rata die vs numeric ==========================================================
expect_identical(x + 2, chronos:::.RataDie(c(-128199, 10958, 391175, 210015, 412359) + 2))
expect_identical(x - 2, chronos:::.RataDie(c(-128199, 10958, 391175, 210015, 412359) - 2))
expect_identical(x * 2, chronos:::.RataDie(c(-128199, 10958, 391175, 210015, 412359) * 2))
expect_identical(x / 2, chronos:::.RataDie(c(-128199, 10958, 391175, 210015, 412359) / 2))

## Not rata die anymore
expect_identical(x ^ 2, c(-128199, 10958, 391175, 210015, 412359)^2)

# numeric vs Rata die ==========================================================
expect_identical(2 + x, chronos:::.RataDie(2 + c(-128199, 10958, 391175, 210015, 412359)))
expect_identical(2 - x, chronos:::.RataDie(2 - c(-128199, 10958, 391175, 210015, 412359)))
expect_identical(2 * x, chronos:::.RataDie(2 * c(-128199, 10958, 391175, 210015, 412359)))

## Not rata die anymore
expect_identical(2 / x, 2 / c(-128199, 10958, 391175, 210015, 412359))
