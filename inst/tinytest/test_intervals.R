Sys.setenv(LANGUAGE = "en") # Force locale

# Create =======================================================================
lower <- c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
           1325, 1375, 1200, 1300, 1375, 1275, 1325)
upper <- c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
           1400, 1500, 1300, 1375, 1500, 1325, 1425)

x <- intervals(start = lower, end = upper, calendar = CE())
expect_identical(length(x), 15L)

expect_error(intervals(start = upper, end = lower, calendar = CE()), "is later than")

lower_rd <- fixed(lower, calendar = CE())
upper_rd <- fixed(upper, calendar = CE())
expect_silent(intervals(start = lower_rd, end = upper_rd))
expect_warning(intervals(start = lower_rd, end = upper_rd, calendar = CE()))

# Terminal times ===============================================================
expect_identical(start(x, calendar = CE()), lower)
expect_identical(end(x, calendar = CE()), upper)

# Duration =====================================================================
expect_identical(span(x, calendar = CE()), upper - lower)
expect_identical(span(x, calendar = CE()), span(x, calendar = BP()))

# Overlap ======================================================================
expect_identical(overlap(x, calendar = CE()), overlap(x, calendar = BP()))

# Inf boundaries ===============================================================
y <- intervals(start = c(50, -Inf, -Inf), end = c(Inf, 50, Inf), calendar = CE())
expect_identical(span(y, calendar = CE()), c(Inf, Inf, Inf))
expect_equivalent(
  overlap(y, calendar = CE()),
  matrix(c(Inf, 1, Inf, 1, Inf, Inf, Inf, Inf, Inf), ncol = 3)
)

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

  plot_interval_Inf <- function() plot(y, calendar = CE())
  expect_snapshot_plot(plot_interval_Inf, "plot_interval_Inf")
}
