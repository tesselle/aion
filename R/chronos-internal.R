# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.#'
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @keywords internal
#' @noRd
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}
