# YEAR
#' @include AllGenerics.R
NULL

# Decimal years ================================================================
#' @export
#' @rdname as_decimal
#' @aliases as_decimal,numeric,numeric,numeric,GregorianCalendar-method
setMethod(
  f = "as_decimal",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "GregorianCalendar"),
  definition = function(year, month, day, calendar) {
    ## Shift origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    .as_decimal(year, month, day, calendar)
  }
)

#' @export
#' @rdname as_decimal
#' @aliases as_decimal,numeric,numeric,numeric,JulianCalendar-method
setMethod(
  f = "as_decimal",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "JulianCalendar"),
  definition = function(year, month, day, calendar) {
    .as_decimal(year, month, day, calendar)
  }
)

.as_decimal <- function(year, month, day, calendar) {
  ## Year length in days
  start <- fixed(year, 01, 01, calendar = calendar)
  end <- fixed(year, 12, 31, calendar = calendar)
  total <- end - start + 1

  ## Elapsed time
  date <- fixed(year, month, day, calendar = calendar)
  sofar <- date - start

  unclass(year + sofar / total)
}

# Leap year ====================================================================
is_julian_leap_year <- function(year) {
  year <- floor(year) # Drop decimal part (if any)
  leap <- year %% 4 == 3
  leap[year > 0] <- year[year > 0] %% 4 == 0
  leap
}

is_gregorian_leap_year <- function(year) {
  year <- floor(year) # Drop decimal part (if any)
  ((year %% 4) == 0) &
    (year %% 400 != 100) &
    (year %% 400 != 200) &
    (year %% 400 != 300)
}
