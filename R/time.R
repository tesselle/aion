# TIME
#' @include AllGenerics.R
NULL

#' @export
#' @rdname start
#' @aliases start,RataDie-method
setMethod(
  f = "start",
  signature = "RataDie",
  definition = function(x) min(x)
)

#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod(
  f = "start",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(x@time)
)


#' @export
#' @rdname start
#' @aliases end,RataDie-method
setMethod(
  f = "end",
  signature = "RataDie",
  definition = function(x) max(x)
)

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod(
  f = "end",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(x@time)
)

#' @export
#' @rdname time
#' @aliases time,RataDie-method
setMethod(
  f = "time",
  signature = "RataDie",
  definition = function(x) methods::as(x, "numeric", strict=TRUE)
)

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod(
  f = "time",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(x@time)
)

#' @export
#' @rdname time
#' @aliases frequency,RataDie-method
setMethod(
  f = "frequency",
  signature = "RataDie",
  definition = function(x) abs(1 / diff(time(x)))
)

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(x@time)
)

#' @export
#' @rdname span
#' @aliases span,RataDie-method
setMethod(
  f = "span",
  signature = "RataDie",
  definition = function(x) max(x) - min(x)
)

#' @export
#' @rdname span
#' @aliases span,TimeSeries-method
setMethod(
  f = "span",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(x@time)
)
