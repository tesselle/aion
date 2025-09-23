Sys.setenv(LANGUAGE = "en") # Force locale

# Create =======================================================================
lower <- c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
           1325, 1375, 1200, 1300, 1375, 1275, 1325)
upper <- c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
           1400, 1500, 1300, 1375, 1500, 1325, 1425)

x <- intervals(start = lower, end = upper, calendar = CE())
expect_identical(length(x), 15L)

expect_error(names(x) <- LETTERS[1:3])
names(x) <- LETTERS[1:15]
expect_identical(names(x), LETTERS[1:15])
expect_identical(labels(x), LETTERS[1:15])
names(x) <- NULL
expect_identical(names(x), paste0("I", 1:15))
expect_identical(labels(x), paste0("I", 1:15))

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

names(x) <- c("I1", "I1", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "I13", "I14", "I15")
over <- overlap(x, calendar = CE(), aggregate = TRUE)
expect_identical(over[c(1, 5), 1], c(I1 = 352, I6 = 77))

# Inf boundaries ===============================================================
y <- intervals(start = c(50, -Inf, -Inf), end = c(Inf, 50, Inf), calendar = CE())
expect_identical(span(y, calendar = CE()), c(Inf, Inf, Inf))
expect_equivalent(
  overlap(y, calendar = CE()),
  matrix(c(Inf, 1, Inf, 1, Inf, Inf, Inf, Inf, Inf), ncol = 3)
)

# Plot =========================================================================
using("tinysnapshot")
source("helpers.R")

x <- intervals(start = lower, end = upper, calendar = CE())

plot_interval_rd <- function() plot(x, calendar = NULL)
expect_snapshot_plot(plot_interval_rd, "plot_interval_rd")

plot_interval_CE <- function() plot(x, calendar = CE())
expect_snapshot_plot(plot_interval_CE, "plot_interval_CE")

plot_interval_Inf <- function() plot(y, calendar = CE())
expect_snapshot_plot(plot_interval_Inf, "plot_interval_Inf")

grp <- c("A", "A", "B", "B", "B", "A", "D", "D", "D", "C", "C", "C", "C", "B", "B")
plot_interval_groups <- function() plot(x, calendar = CE(), groups = grp)
expect_snapshot_plot(plot_interval_CE, "plot_interval_groups")
