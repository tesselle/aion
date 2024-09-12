# SPAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname span
#' @aliases span,TimeSeries-method
setMethod(
  f = "span",
  signature = "TimeSeries",
  definition = function(x, calendar = NULL) {
    z <- max(x@.Time) - min(x@.Time)
    if (is.null(calendar)) return(unclass(z))
    as_year(z, calendar = calendar, shift = FALSE)
  }
)

#' @export
#' @rdname span
#' @aliases span,TimeIntervals-method
setMethod(
  f = "span",
  signature = "TimeIntervals",
  definition = function(x, calendar = NULL) {
    z <- x@.End - x@.Start
    if (is.null(calendar)) return(unclass(z))
    as_year(z, calendar = calendar, shift = FALSE)
  }
)
