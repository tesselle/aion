# RataDie ======================================================================
x <- fixed(c(-350, 31, 1072, 576, 1130), calendar = CE())

X <- x[1:3]
expect_identical(length(X), 3L)

# TimeSeries ===================================================================
a <- matrix(rnorm(300), 100, 3)
colnames(a) <- c("A", "B", "C")
b <- series(a, time = 1000:1099, calendar = BCE())

M <- b[, , , drop = TRUE]
expect_identical(M, b@.Data[, , , drop = TRUE])

V <- b[1, , , drop = TRUE]
expect_identical(V, b@.Data[1, , ])

Y <- b[1:5, , ]
expect_identical(time(Y), time(b)[1:5])
expect_identical(dim(Y), c(5L, 3L, 1L))
expect_identical(colnames(Y), colnames(a))

Z <- b[, 1:2, ]
expect_identical(time(Z), time(b))
expect_identical(dim(Z), c(100L, 2L, 1L))
expect_identical(colnames(Z), c("A", "B"))
