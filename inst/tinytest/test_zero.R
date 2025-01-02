Sys.setenv(LANGUAGE = "en") # Force locale

# Year zero ====================================================================
## Julian calendar -------------------------------------------------------------
y <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
expect_error(fixed(y, calendar = J()), "There is no year zero")

if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  X <- series(
    object = matrix(sin(1:306), nrow = 51, ncol = 6),
    time = seq(from = -25, to = 25, length.out = 51),
    calendar = CE()
  )

  plot_zero_Julian <- function() plot(X, calendar = J())
  expect_snapshot_plot(plot_zero_Julian, "plot_zero_Julian")
}
