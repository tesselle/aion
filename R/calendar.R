# CALENDAR
#' @include AllGenerics.R
NULL

# Gregorian calendar ===========================================================
#' @export
#' @rdname gregorian
gregorian <- function(label, name, epoch, direction) {
  .GregorianCalendar(
    label = label,
    name = name,
    epoch = epoch,
    direction = as.integer(sign(direction))
  )
}

#' @export
#' @rdname gregorian
#' @aliases as_gregorian,JulianCalendar-method
setMethod(
  f = "as_gregorian",
  signature = "JulianCalendar",
  definition = function(object) {
    # TODO
  }
)

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

# Juliab calendar ==============================================================
#' @export
#' @rdname julian
#' @aliases as_julian,GregorianCalendar-method
setMethod(
  f = "as_julian",
  signature = "GregorianCalendar",
  definition = function(object) {
    # TODO
  }
)

#' @export
#' @rdname julian
#' @aliases is_julian,TimeScale-method
setMethod(
  f = "is_julian",
  signature = "TimeScale",
  definition = function(object) {
    methods::is(object, "JulianCalendar")
  }
)
