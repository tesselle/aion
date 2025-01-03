# GREGORIAN CALENDAR
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

    is_leap <- which(is_gregorian_leap_year(year))
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

    ## Fix infinite values
    rd[is.infinite(year)] <- year[is.infinite(year)]

    .RataDie(rd)
  }
)
# Gregorian from fixed =========================================================
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

    ## Fix infinite values
    year[is.infinite(object)] <- object[is.infinite(object)]

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

# Era ==========================================================================
#' @export
#' @rdname fixed_gregorian
fixed_from_BP <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = BP())
  else fixed(year, month, day, calendar = BP())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_BP <- function(object) {
  as_year(object, calendar = BP(), decimal = TRUE)
}

#' @export
#' @rdname fixed_gregorian
fixed_from_BC <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = BC())
  else fixed(year, month, day, calendar = BC())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_BC <- function(object) {
  as_year(object, calendar = BC(), decimal = TRUE)
}

#' @export
#' @rdname fixed_gregorian
fixed_from_BCE <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = BCE())
  else fixed(year, month, day, calendar = BCE())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_BCE <- function(object) {
  as_year(object, calendar = BCE(), decimal = TRUE)
}

#' @export
#' @rdname fixed_gregorian
fixed_from_AD <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = AD())
  else fixed(year, month, day, calendar = AD())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_AD <- function(object) {
  as_year(object, calendar = AD(), decimal = TRUE)
}

#' @export
#' @rdname fixed_gregorian
fixed_from_CE <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = CE())
  else fixed(year, month, day, calendar = CE())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_CE <- function(object) {
  as_year(object, calendar = CE(), decimal = TRUE)
}

#' @export
#' @rdname fixed_gregorian
fixed_from_b2k <- function(year, month, day) {
  if (missing(month) || missing(day)) fixed(year, calendar = b2k())
  else fixed(year, month, day, calendar = b2k())
}
#' @export
#' @rdname fixed_gregorian
fixed_to_b2k <- function(object) {
  as_year(object, calendar = b2k(), decimal = TRUE)
}
