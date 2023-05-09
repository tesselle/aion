# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

# Helpers ======================================================================
#' Fixed from Moment
#'
#' Convert a moment Into an r.d. date
#' @param t A [`numeric`] vector of moment (i.e. a fixed date that has a
#'  fractional part).
#' @return
#'  A [`numeric`] vector of fixed dates.
#' @references
#'  Reingold2018, p. 20.
#' @keywords internal
#' @noRd
fixed_from_moment <- function(t) {
  floor(t)
}

#' Time from Moment
#'
#' @param t A [`numeric`] vector of moment (i.e. a fixed date that has a
#'  fractional part).
#' @return
#'  A [`numeric`] vector of time (i.e. the fractional part of `t`).
#' @references
#'  Reingold2018, p. 21.
#' @keywords internal
#' @noRd
time_from_moment <- function(t) {
  t %% 1
}
