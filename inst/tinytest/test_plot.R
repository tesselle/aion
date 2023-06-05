if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  # Plot multiple ==============================================================
  X <- series(
    object = matrix(sin(1:300), nrow = 50, ncol = 6),
    time = seq(2000, by = 2, length.out = 50),
    calendar = calendar("BP")
  )

  plot_facet_CE <- function() plot(X)
  expect_snapshot_plot(plot_facet_CE, "plot_facet_CE")

  plot_facet_BP <- function() plot(X, calendar = BP(), flip = TRUE)
  expect_snapshot_plot(plot_facet_BP, "plot_facet_BP")

  plot_facet_b2k <- function() plot(X, calendar = b2k(), ncol = 1)
  expect_snapshot_plot(plot_facet_b2k, "plot_facet_b2k")

  # Plot single ================================================================
  plot_single <- function() plot(X, type = "single", calendar = BP(),
                                 col = rainbow(6))
  expect_snapshot_plot(plot_single, "plot_single")

  # Image ======================================================================
  plot_image <- function() image(X, calendar = BP())
  expect_snapshot_plot(plot_image, "plot_image")

  # Axis and grid ==============================================================
  ## Vector of years expressed in ka BP
  x <- fixed(c(30, 35, 40), calendar = BP(), scale = 1000)

  axis_default <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    year_axis(side = 1, x = x, calendar = CE())
    year_grid(calendar = CE())
  }
  expect_snapshot_plot(axis_default, "axis_default")

  axis_ka <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    year_axis(side = 1, x = x, format = "ka", calendar = CE())
    year_grid(6, NA, calendar = CE())
  }
  expect_snapshot_plot(axis_ka, "axis_ka")

  axis_Ma <- function() {
    plot(NA, xlim = range(x), ylim = c(0, 1), xaxt = "n")
    year_axis(side = 1, x = x, format = "Ma", calendar = CE())
    year_grid(NA, 6, calendar = CE())
  }
  expect_snapshot_plot(axis_Ma, "axis_Ma")
}
