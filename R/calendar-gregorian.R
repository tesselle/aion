# GREGORIAN CALENDAR
#' @include AllGenerics.R
NULL

# Gregorian calendar ===========================================================
#' @export
#' @rdname is
#' @aliases is_gregorian,ANY-method
setMethod(
  f = "is_gregorian",
  signature = "ANY",
  definition = function(object) {
    methods::is(object, "GregorianCalendar")
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

# Helpers ======================================================================
is_gregorian_leap_year <- function(year) {
  year <- floor(year) # Drop decimal part (if any)
  ((year %% 4) == 0) &
    (year %% 400 != 100) &
    (year %% 400 != 200) &
    (year %% 400 != 300)
}
