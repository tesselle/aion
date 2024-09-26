# YEAR
#' @include AllGenerics.R
NULL

# JulianCalendar ===============================================================
#' @export
#' @rdname as_year
#' @aliases as_year,numeric,JulianCalendar-method
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "JulianCalendar"),
  definition = function(object, calendar, ...) {
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

# GregorianCalendar ============================================================
#' @export
#' @rdname as_year
#' @aliases as_year,numeric,GregorianCalendar-method
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "GregorianCalendar"),
  definition = function(object, calendar, decimal = TRUE, ...) {
    d0 <- object - calendar_fixed(calendar)
    n400 <- d0 %/% 146097
    d1 <- d0 %% 146097
    n100 <- d1 %/% 36524
    d2 <- d1 %% 36524
    n4 <- d2 %/% 1461
    d3 <- d2 %% 1461
    n1 <- d3 %/% 365

    year <- 400 * n400 + 100 * n100 + 4 * n4 + n1
    year <- ifelse(n100 == 4 | n1 == 4, year, year + 1)

    ## Shift origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    if (isTRUE(decimal)) {
      ## Year length in days
      start <- fixed(year, 01, 01, calendar = calendar)
      end <- fixed(year, 12, 31, calendar = calendar)
      total <- end - start + 1

      ## Elapsed time
      sofar <- object - start

      year <- year + sofar / total
    }

    year
  }
)

#' @export
#' @rdname as_date
#' @aliases as_date,numeric,GregorianCalendar-method
setMethod(
  f = "as_date",
  signature = c(object = "numeric", calendar = "GregorianCalendar"),
  definition = function(object, calendar) {
    year <- as_year(object, calendar = calendar, decimal = FALSE)
    prior_days <- object - fixed(year, 01, 01, calendar = calendar)

    correction <- rep(2, length(object))
    correction[object < fixed(year, 03, 01, calendar = calendar)] <- 0
    correction[is_gregorian_leap_year(year)] <- 1

    month <- (12 * (prior_days + correction) + 373) %/% 367
    day <- object - fixed(year, month, 01, calendar = calendar) + 1

    data.frame(
      year = as.numeric(year),
      month = as.numeric(month),
      day = as.numeric(day)
    )
  }
)
