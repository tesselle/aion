# Create from vector ===========================================================
x <- rnorm(91)

X <- series(x, time = seq(100, 109, 0.1), calendar = calendar("BCE"), scale = 10)
Y <- series(x, fixed(1000:1090, calendar = calendar("BCE")))
expect_identical(X, Y)

# Create from matrix ===========================================================
x <- matrix(rnorm(300), 100, 3)

X <- series(x, time = 1000:1099, calendar = calendar("BCE"), names = c("A", "B", "C"))
Y <- series(x, time = fixed(1000:1099, calendar = calendar("BCE")), names = c("A", "B", "C"))
expect_identical(X, Y)

# Create from data.frame =======================================================
x <- as.data.frame(matrix(rnorm(300), 100, 3))

X <- series(x, time = 1000:1099, calendar = calendar("BCE"), scale = 10)
Y <- series(x, time = fixed(1000:1099, calendar = calendar("BCE"), scale = 10))
expect_identical(X, Y)

# Chronological order ==========================================================
x <- matrix(rnorm(300), 100, 3)
rd <- fixed(year = 1000:1099, calendar = calendar("BCE"))

X <- series(x, time = rd)
expect_equal(as.numeric(X@time), sort(rd@.Data))

i <- sample(100)
Y <- series(x[i, ], time = rd[i]) # Reorder
expect_identical(X, Y)

expect_warning(series(x, time = rd, calendar = CE()), "expressed in rata die")

# Coerce to data.frame =========================================================
X <- series(
  object = matrix(rnorm(300), nrow = 50, ncol = 6),
  time = seq(from = 2000, by = -2, length.out = 50),
  calendar = calendar("BP")
)
df <- as.data.frame(X, calendar = b2k())
expect_equal(ncol(df), 4)
expect_equal(df$time, rep(time(X, calendar = b2k()), 6))
