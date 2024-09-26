# JULINA CALENDAR
#' @include AllGenerics.R
NULL

# Juliab calendar ==============================================================
#' @export
#' @rdname is
#' @aliases is_julian,ANY-method
setMethod(
  f = "is_julian",
  signature = "ANY",
  definition = function(object) {
    methods::is(object, "JulianCalendar")
  }
)

# Era ==========================================================================
#' @export
#' @rdname fixed_julian
fixed_from_julian <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = J())
  else fixed(year, month, day, calendar = J())
}
#' @export
#' @rdname fixed_julian
fixed_to_julian <- function(object) {
  as_year(object, calendar = J())
}

# Helpers ======================================================================
is_julian_leap_year <- function(year) {
  year <- floor(year) # Drop decimal part (if any)
  leap <- year %% 4 == 3
  leap[year > 0] <- year[year > 0] %% 4 == 0
  leap
}
