# Create =======================================================================
lower <- c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
           1325, 1375, 1200, 1300, 1375, 1275, 1325)
upper <- c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
           1400, 1500, 1300, 1375, 1500, 1325, 1425)

x <- intervals(start = lower, end = upper, calendar = CE())

# Terminal times ===============================================================
expect_identical(start(x, calendar = CE()), lower)
expect_identical(end(x, calendar = CE()), upper)

# Duration =====================================================================
expect_identical(span(x, calendar = CE()), span(x, calendar = BP()))

# Overlap ======================================================================
expect_identical(overlap(x, calendar = CE()), overlap(x, calendar = BP()))

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_interval_rd <- function() plot(x, calendar = NULL)
  expect_snapshot_plot(plot_interval_rd, "plot_interval_rd")

  plot_interval_CE <- function() plot(x, calendar = CE())
  expect_snapshot_plot(plot_interval_CE, "plot_interval_CE")
}
