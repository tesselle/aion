# GREGORIAN CALENDAR
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
#' @aliases is_gregorian,TimeScale-method
setMethod(
  f = "is_gregorian",
  signature = "TimeScale",
  definition = function(object) {
    methods::is(object, "GregorianCalendar")
  }
)
