# TIME
#' @include AllGenerics.R
NULL

#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod(
  f = "start",
  signature = "TimeSeries",
  definition = function(x, calendar = NULL) {
    z <- min(x@.Time)
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname start
#' @aliases start,TimeIntervals-method
setMethod(
  f = "start",
  signature = "TimeIntervals",
  definition = function(x, calendar = NULL) {
    z <- x@.Start
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod(
  f = "end",
  signature = "TimeSeries",
  definition = function(x, calendar = NULL) {
    z <- max(x@.Time)
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname start
#' @aliases end,TimeIntervals-method
setMethod(
  f = "end",
  signature = "TimeIntervals",
  definition = function(x, calendar = NULL) {
    z <- x@.End
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod(
  f = "time",
  signature = "TimeSeries",
  definition = function(x, calendar = NULL) {
    z <- x@.Time
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) mean(abs(1 / diff(time(x))))
)
