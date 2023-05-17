# TIME
#' @include AllGenerics.R
NULL

#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod(
  f = "start",
  signature = "TimeSeries",
  definition = function(x) min(x@time)
)

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod(
  f = "end",
  signature = "TimeSeries",
  definition = function(x) max(x@time)
)

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod(
  f = "time",
  signature = "TimeSeries",
  definition = function(x) methods::as(x@time, "numeric", strict=TRUE)
)

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) mean(abs(1 / diff(time(x))))
)

#' @export
#' @rdname time
#' @aliases years,TimeSeries-method
setMethod(
  f = "years",
  signature = "TimeSeries",
  definition = function(x, calendar = getOption("chronos.calendar")) {
    as_year(x@time, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname span
#' @aliases span,TimeSeries-method
setMethod(
  f = "span",
  signature = "TimeSeries",
  definition = function(x) max(x@time) - min(x@time)
)
