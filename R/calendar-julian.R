# JULINA CALENDAR
#' @include AllGenerics.R
NULL

# Juliab calendar ==============================================================
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
