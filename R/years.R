# YEAR VECTORS
#' @include AllGenerics.R
NULL

as_decimal = function(year, month, day, calendar) {
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

# Gregorian ====================================================================
#' @export
#' @rdname as_year
#' @aliases as_year,numeric,GregorianCalendar-method
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "GregorianCalendar"),
  definition = function(object, calendar) {
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
  definition = function(object, calendar) {
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
