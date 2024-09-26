# SPAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname span
#' @aliases span,TimeSeries-method
setMethod(
  f = "span",
  signature = c(x = "TimeSeries"),
  definition = function(x, calendar = NULL) {
    z <- end(x, calendar = calendar) - start(x, calendar = calendar)
    unclass(z) * calendar_direction(calendar)
  }
)

#' @export
#' @rdname span
#' @aliases span,TimeIntervals-method
setMethod(
  f = "span",
  signature = c(x = "TimeIntervals"),
  definition = function(x, calendar = NULL) {
    z <- end(x, calendar = calendar) - start(x, calendar = calendar)
    unclass(z) * calendar_direction(calendar)
  }
)
