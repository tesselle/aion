# GREGORIAN CALENDAR
#' @include AllGenerics.R
NULL

# Gregorian calendar ===========================================================
#' @export
#' @rdname gregorian
#' @aliases is_gregorian,TimeScale-method
setMethod(
  f = "is_gregorian",
  signature = "TimeScale",
  definition = function(object) {
    methods::is(object, "GregorianCalendar")
  }
)

# Fixed from Gregorian =========================================================
#' @export
#' @rdname fixed
#' @aliases fixed,numeric,missing,missing,GregorianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "missing", day = "missing", calendar = "GregorianCalendar"),
  definition = function(year, calendar, scale = 1) {
    ## Rescale to years (if not already)
    year <- year * scale

    methods::callGeneric(year = year, month = 01, day = 01, calendar = calendar)
  }
)

#' @export
#' @rdname fixed
#' @aliases fixed,numeric,numeric,numeric,GregorianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "GregorianCalendar"),
  definition = function(year, month, day, calendar) {
    ## Switch origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    # Correct for 28- or 29-day Feb
    correction <- ifelse(is_gregorian_leap_year(year), -1, -2)
    correction <- ifelse(month <= 2, 0, correction)

    rd <- calendar_fixed(calendar) - 1 +   # Days before start of calendar
      365 * (year - 1) +        # Ordinary days since epoch
      floor((year - 1) / 4) -   # Julian leap days since epoch minus...
      floor((year - 1) / 100) + # ...century years since epoch plus...
      floor((year - 1) / 400) + # ...years since epoch divisible by 400
      floor((1 / 12) * (367 * month - 362)) + # Days in prior months this year assuming 30-day Feb
      correction +              # Correct for 28- or 29-day Feb
      day                       # Days so far this month.

    .RataDie(rd)
  }
)

from_BP <- function(year, month, day) {
  fixed(year, month, day, calendar = BP())
}
from_BC <- function(year, month, day) {
  fixed(year, month, day, calendar = BC())
}
from_BCE <- function(year, month, day) {
  fixed(year, month, day, calendar = BCE())
}
from_AD <- function(year, month, day) {
  fixed(year, month, day, calendar = AD())
}
from_CE <- function(year, month, day) {
  fixed(year, month, day, calendar = CE())
}
from_b2k <- function(year, month, day) {
  fixed(year, month, day, calendar = b2k())
}

# Helpers ======================================================================
is_gregorian_leap_year <- function(year) {
  ((year %% 4) == 0) &
    (year %% 400 != 100) &
    (year %% 400 != 200) &
    (year %% 400 != 300)
}
