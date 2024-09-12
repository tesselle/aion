# HELPERS

make_par <- function(params, x, n = 1) {
  p <- params[[x]] %||% graphics::par()[[x]]
  if (length(p) == 1 && n > 1) p <- rep(p, length.out = n)
  p
}

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`.
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @note For internal use only.
#' @family graphic tools
#' @keywords internal
#' @noRd
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}
