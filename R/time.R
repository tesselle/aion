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
    z <- min(x@time)
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
    z <- max(x@time)
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
    z <- x@time
    if (is.null(calendar)) return(methods::as(z, "numeric", strict=TRUE))
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

#' @export
#' @rdname span
#' @aliases span,TimeSeries-method
setMethod(
  f = "span",
  signature = "TimeSeries",
  definition = function(x, calendar = NULL) {
    z <- max(x@time) - min(x@time)
    if (is.null(calendar)) return(z)
    as_year(z, calendar = calendar, decimal = TRUE)
  }
)
