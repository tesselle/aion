# HELPERS

make_par <- function(params, x, n = 0) {
  p <- params[[x]] %||% graphics::par()[[x]]
  if (n > 0) p <- rep(p, length.out = n)
  p
}
