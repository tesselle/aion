# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calendar
#' @aliases calendar,character-method
setMethod(
  f = "calendar",
  signature = "character",
  definition = function(object) {
    switch (
      tolower(object),
      bp = .BP(),
      b2k = .b2k(),
      bc = .BC(),
      bce = .BCE(),
      ad = .AD(),
      ce = .CE(),
      julian = .JulianCalendar(),
      stop(sprintf("Unknown calendar: %s", object), call. = FALSE)
    )
  }
)

#' @export
#' @rdname gregorian
BP <- function(...) calendar("BP")

#' @export
#' @rdname gregorian
b2k <- function(...) calendar("b2k")

#' @export
#' @rdname gregorian
BC <- function(...) calendar("BC")

#' @export
#' @rdname gregorian
BCE <- function(...) calendar("BCE")

#' @export
#' @rdname gregorian
AD <- function(...) calendar("AD")

#' @export
#' @rdname gregorian
CE <- function(...) calendar("CE")

#' @export
#' @rdname julian
J <- function(...) calendar("julian")

# Mutators =====================================================================
calendar_fixed <- function(object) object@fixed
calendar_year <- function(object) object@year

## Getters ---------------------------------------------------------------------
#' @export
#' @rdname calendar_get
#' @aliases calendar_label,TimeScale-method
setMethod(
  f = "calendar_label",
  signature = "TimeScale",
  definition = function(object) object@label
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,TimeScale-method
setMethod(
  f = "calendar_name",
  signature = "TimeScale",
  definition = function(object) object@name
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_unit,TimeScale-method
setMethod(
  f = "calendar_unit",
  signature = "TimeScale",
  definition = function(object) {
    if (is_gregorian(object)) return("Gregorian")
    if (is_julian(object)) return("Julian")
    return("Undefined")
  }
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,TimeScale-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeScale",
  definition = function(object) object@epoch
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeScale-method
setMethod(
  f = "calendar_direction",
  signature = "TimeScale",
  definition = function(object) sign(object@direction)
)
