# GREGORIAN CALENDAR
#' @include AllGenerics.R
NULL

# Gregorian calendar ===========================================================
#' @export
#' @rdname gregorian
as_gregorian <- function(label, name, epoch, direction) {
  .GregorianCalendar(
    label = label,
    name = name,
    epoch = epoch,
    direction = as.integer(sign(direction))
  )
}

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

# Gregorian from fixed =========================================================
#' @export
#' @rdname as_year
#' @aliases as_year,numeric,GregorianCalendar-method
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "GregorianCalendar"),
  definition = function(object, calendar = calendar("CE")) {
    d0 <- object - fixed(calendar)
    n400 <- floor(d0 / 146097)
    d1 <- d0 %% 146097
    n100 <- floor(d1 / 36524)
    d2 <- d1 %% 36524
    n4 <- floor(d2 / 1461)
    d3 <- d2 %% 1461
    n1 <- floor(d3 / 365)

    year <- 400 * n400 + 100 * n100 + 4 * n4 + n1
    year <- ifelse(n100 == 4 | n1 == 4, year, year + 1)

    (year - calendar_epoch(calendar)) * calendar_direction(calendar)
  }
)

#' @export
#' @rdname as_date
#' @aliases as_date,numeric,GregorianCalendar-method
setMethod(
  f = "as_date",
  signature = c(object = "numeric", calendar = "GregorianCalendar"),
  definition = function(object, calendar = calendar("CE")) {
    year <- as_year(object, calendar = calendar)
    prior_days <- object - as_fixed(year, 01, 01, calendar = calendar)

    correction <- 2
    ifelse(object < as_fixed(year, 03, 01, calendar = calendar), 0, correction)
    ifelse(is_gregorian_leap_year(year), 1, correction)

    month <- floor((1 / 367) * (12 * (prior_days + correction) + 373))
    day <- object - as_fixed(year, month, 01, calendar = calendar) + 1

    data.frame(
      year = unclass(year),
      month = unclass(month),
      day = unclass(day)
    )
  }
)

# Fixed from Gregorian =========================================================
as_decimal = function(year, month, day, calendar = calendar("CE")) {
  ## Switch origin
  year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

  ## Year length in days
  start <- as_fixed(year, 01, 01, calendar = calendar)
  end <- as_fixed(year, 12, 31, calendar = calendar)
  total <- end - start + 1

  ## Elapsed time
  date <- as_fixed(year, month, day, calendar = calendar)
  sofar <- date - start

  unclass(year + sofar / total)
}

#' @export
#' @rdname as_fixed
#' @aliases as_fixed,numeric,missing,missing,GregorianCalendar-method
setMethod(
  f = "as_fixed",
  signature = c(year = "numeric", month = "missing", day = "missing", calendar = "GregorianCalendar"),
  definition = function(year, calendar = calendar("CE")) {
    methods::callGeneric(year = year, month = 01, day = 01, calendar = calendar)
  }
)

#' @export
#' @rdname as_fixed
#' @aliases as_fixed,numeric,numeric,numeric,GregorianCalendar-method
setMethod(
  f = "as_fixed",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "GregorianCalendar"),
  definition = function(year, month, day, calendar = calendar("CE")) {
    ## Switch origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    # Correct for 28- or 29-day Feb
    correction <- ifelse(is_gregorian_leap_year(year), -1, -2)
    correction <- ifelse(month <= 2, 0, correction)

    rd <- fixed(calendar) - 1 +   # Days before start of calendar
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

# Helpers ======================================================================
is_gregorian_leap_year <- function(year) {
  ((year %% 4) == 0) &
    (year %% 400 != 100) &
    (year %% 400 != 200) &
    (year %% 400 != 300)
}
