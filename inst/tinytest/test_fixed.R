# Rata die =====================================================================
x <- fixed(1000:1099, calendar = calendar("BCE"))
y <- as_fixed(x@.Data)
expect_identical(x, y)
