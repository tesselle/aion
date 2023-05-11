test_that("Plot facets", {
  X <- series(
    object = matrix(sin(1:300), nrow = 50, ncol = 6),
    time = seq(2000, by = 2, length.out = 50),
    calendar = calendar("BP")
  )

  plot_facet_CE <- function() plot(X)
  vdiffr::expect_doppelganger("plot_facet_CE", plot_facet_CE)

  plot_facet_BP <- function() plot(X, calendar = calendar("BP"))
  vdiffr::expect_doppelganger("plot_facet_BP", plot_facet_BP)

  plot_facet_b2k <- function() plot(X, calendar = calendar("b2k"))
  vdiffr::expect_doppelganger("plot_facet_b2k", plot_facet_b2k)
})
test_that("Axis", {
  ## Vector of years expressed in ka BP
  x <- fixed(c(30, 35, 40), calendar = calendar("BP"), scale = 1000)

  axis_default <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    axis_year(side = 1, x = x)
  }
  vdiffr::expect_doppelganger("axis_default", axis_default)

  axis_ka <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    axis_year(side = 1, x = x, format = "ka")
  }
  vdiffr::expect_doppelganger("axis_ka", axis_ka)

  axis_Ma <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    axis_year(side = 1, x = x, format = "Ma")
  }
  vdiffr::expect_doppelganger("axis_Ma", axis_Ma)
})
