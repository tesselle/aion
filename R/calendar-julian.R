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

# Julian from fixed ============================================================
#' @export
#' @rdname as_year
#' @aliases as_year,numeric,JulianCalendar-method
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "JulianCalendar"),
  definition = function(object, calendar) {
    d0 <- object - calendar_fixed(calendar)
    year <- (4 * d0 + 1464) %/% 1461

    ## There is no year 0 on the Julian calendar
    year[year <= 0] <- year[year <= 0] - 1

    unclass(year)
  }
)

#' @export
#' @rdname as_date
#' @aliases as_date,numeric,JulianCalendar-method
setMethod(
  f = "as_date",
  signature = c(object = "numeric", calendar = "JulianCalendar"),
  definition = function(object, calendar) {
    year <- as_year(object, calendar = calendar)
    prior_days <- object - fixed(year, 01, 01, calendar = calendar)

    correction <- rep(2, length(object))
    correction[object < fixed(year, 03, 01, calendar = calendar)] <- 0
    correction[is_julian_leap_year(year)] <- 1

    month <- (12 * (prior_days + correction) + 373) %/% 367
    day <- object - fixed(year, month, 01, calendar = calendar) + 1

    data.frame(
      year = unclass(year),
      month = unclass(month),
      day = unclass(day)
    )
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
