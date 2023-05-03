# test_that("Plot facets", {
#   X <- series(
#     object = matrix(rnorm(300), nrow = 50, ncol = 6),
#     time = seq(2000, by = 2, length.out = 50),
#     calendar = era("BP")
#   )
#
#   plot_facet <- function() plot(X)
#   vdiffr::expect_doppelganger("plot_facet", plot_facet)
# })
