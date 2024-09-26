# RATA DIE
#' @include AllGenerics.R
NULL

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

    rd <- fixed(year, 01, 01, calendar = calendar)

    is_leap <- is_gregorian_leap_year(year)
    rd[is_leap] <- ceiling(rd[is_leap]) # WHY ???
    rd
  }
)

#' @export
#' @rdname fixed
#' @aliases fixed,numeric,numeric,numeric,GregorianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "GregorianCalendar"),
  definition = function(year, month, day, calendar) {
    ## Recycle
    n <- length(year)
    if (n > 1) {
      if (length(month) == 1) month <- rep(month, n)
      if (length(day) == 1) day <- rep(day, n)
    }

    ## Switch origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    ## Correct for 28- or 29-day Feb
    correction <- rep(-2, length(year))
    correction[is_gregorian_leap_year(year)] <- -1
    correction[month <= 2] <- 0

    rd <- calendar_fixed(calendar) - 1 + # Days before start of calendar
      365 * (year - 1) +                 # Ordinary days since epoch
      (year - 1) %/% 4 -                 # Julian leap days since epoch minus...
      (year - 1) %/% 100 +               # ...century years since epoch plus...
      (year - 1) %/% 400 +               # ...years since epoch divisible by 400
      (367 * month - 362) %/% 12 +       # Days in prior months this year assuming 30-day Feb
      correction +                       # Correct for 28- or 29-day Feb
      day                                # Days so far this month.

    .RataDie(rd)
  }
)

# Fixed from Julian ============================================================
#' @export
#' @rdname fixed
#' @aliases fixed,numeric,numeric,numeric,JulianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "JulianCalendar"),
  definition = function(year, month, day, calendar) {
    ## Correct for 28- or 29-day Feb
    correction <- rep(-2, length(year))
    correction[is_julian_leap_year(year)] <- -1
    correction[month <= 2] <- 0

    ## There is no year 0 on the Julian calendar
    year[year < 0] <- year[year < 0] + 1

    rd <- calendar_fixed(calendar) - 1 + # Days before start of calendar
      365 * (year - 1) +                 # Ordinary days since epoch
      (year - 1) %/% 4 +                 # Leap days since epoch
      (367 * month - 362) %/% 12 +       # Days in prior months this year assuming 30-day Feb
      correction +                       # Correct for 28- or 29-day Feb
      day                                # Days so far this month.

    .RataDie(rd)
  }
)
