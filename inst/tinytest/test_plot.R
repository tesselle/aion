Sys.setenv(LANGUAGE = "en") # Force locale

using("tinysnapshot")
source("helpers.R")

# Plot multiple ================================================================
X <- series(
  object = matrix(sin(1:300), nrow = 50, ncol = 6),
  time = seq(2000, by = 2, length.out = 50),
  calendar = calendar("BP")
)

plot_facet_rd <- function() plot(X, calendar = NULL)
expect_snapshot_plot(plot_facet_rd, "plot_facet_rd")

plot_facet_CE <- function() plot(X)
expect_snapshot_plot(plot_facet_CE, "plot_facet_CE")

plot_facet_BP <- function() plot(X, calendar = BP(), flip = TRUE)
expect_snapshot_plot(plot_facet_BP, "plot_facet_BP")

plot_facet_b2k <- function() plot(X, calendar = b2k(), ncol = 1)
expect_snapshot_plot(plot_facet_b2k, "plot_facet_b2k")

# Plot single ==================================================================
plot_single <- function() plot(X, facet = "single", calendar = BP())
expect_snapshot_plot(plot_single, "plot_single")

# Graphical parameters  ========================================================
# X <- series(
#   object = array(sin(1:900), dim = c(50, 6, 3)),
#   time = seq(2000, by = 2, length.out = 50),
#   calendar = calendar("BP")
# )
#
# plot(X, lwd = c(1, 2, 3), col = c("#004488", "#DDAA33", "#BB5566"))
# plot(X, type = "b", pch = 16, col = c("#004488", "#DDAA33", "#BB5566"))
# plot(X, type = "p", pch = c(16, 17, 18), cex = c(1, 2, 3))

# Image ========================================================================
plot_image <- function() image(X, calendar = BP())
expect_snapshot_plot(plot_image, "plot_image")

# Axis and grid ================================================================
## Vector of years expressed in ka BP
X <- series(
  object = matrix(sin(1:3), nrow = 3, ncol = 1),
  time = c(30, 35, 40),
  scale = 1000,
  calendar = calendar("BP")
)

axis_default <- function() {
  plot(X, calendar = BP(), axes = FALSE)
  year_axis(side = 1)
  axis(side = 2)
  graphics::grid()
}
expect_snapshot_plot(axis_default, "axis_default")

axis_ka <- function() {
  plot(X, calendar = BP(), axes = FALSE)
  year_axis(side = 1)
  year_axis(side = 3, format = "ka", calendar = CE())
  graphics::grid()
}
expect_snapshot_plot(axis_ka, "axis_ka")

axis_Ma <- function() {
  plot(X, calendar = BP(), axes = FALSE)
  year_axis(side = 1)
  year_axis(side = 3, format = "Ma", calendar = CE())
  graphics::grid()
}
expect_snapshot_plot(axis_Ma, "axis_Ma")
