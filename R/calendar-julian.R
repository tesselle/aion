# JULINA CALENDAR
#' @include AllGenerics.R
NULL

# Fixed from Julian ============================================================
#' @export
#' @rdname fixed
#' @aliases fixed,numeric,missing,missing,JulianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "missing", day = "missing", calendar = "JulianCalendar"),
  definition = function(year, calendar, scale = 1) {
    ## Rescale to years (if not already)
    year <- year * scale

    rd <- fixed(year, 01, 01, calendar = calendar)

    is_leap <- which(is_julian_leap_year(year))
    rd[is_leap] <- ceiling(rd[is_leap]) # WHY ???
    rd
  }
)

#' @export
#' @rdname fixed
#' @aliases fixed,numeric,numeric,numeric,JulianCalendar-method
setMethod(
  f = "fixed",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "JulianCalendar"),
  definition = function(year, month, day, calendar) {
    ## Validation
    if (any(year == 0)) {
      stop(tr_("There is no year zero in the Julian calendar."), call. = FALSE)
    }

    ## Correct for 28- or 29-day Feb
    correction <- rep(-2, length(year))
    correction[which(is_julian_leap_year(year))] <- -1
    correction[month <= 2] <- 0

    ## There is no year 0 on the Julian calendar
    year[year < 0] <- year[year < 0] + 1

    rd <- calendar_fixed(calendar) - 1 + # Days before start of calendar
      365 * (year - 1) +                 # Ordinary days since epoch
      (year - 1) %/% 4 +                 # Leap days since epoch
      (367 * month - 362) %/% 12 +       # Days in prior months this year assuming 30-day Feb
      correction +                       # Correct for 28- or 29-day Feb
      day                                # Days so far this month.

    ## Fix infinite values
    rd[is.infinite(year)] <- year[is.infinite(year)]

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
  definition = function(object, calendar, decimal = FALSE, ...) {
    d0 <- object - calendar_fixed(calendar)
    year <- (4 * d0 + 1464) %/% 1461

    ## There is no year 0 on the Julian calendar
    year[year <= 0] <- year[year <= 0] - 1

    if (isTRUE(decimal)) {
      ## Year length in days
      start <- fixed(year, 01, 01, calendar = calendar)
      end <- fixed(year, 12, 31, calendar = calendar)
      total <- end - start + 1

      ## Elapsed time
      sofar <- object - start

      year <- year + sofar / total
    }

    ## Fix infinite values
    year[is.infinite(object)] <- object[is.infinite(object)]

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
    year <- as_year(object, calendar = calendar, decimal = FALSE)
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
