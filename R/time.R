# TIME
#' @include AllGenerics.R
NULL

#' @export
#' @rdname start
#' @aliases start,TimeLine-method
setMethod(
  f = "start",
  signature = "TimeLine",
  definition = function(x) {
    min(x * calendar_direction(x), na.rm = TRUE) * calendar_direction(x)
  }
)

#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod(
  f = "start",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(years(x))
)


#' @export
#' @rdname start
#' @aliases end,TimeLine-method
setMethod(
  f = "end",
  signature = "TimeLine",
  definition = function(x) {
    max(x * calendar_direction(x), na.rm = TRUE) * calendar_direction(x)
  }
)

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod(
  f = "end",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(years(x))
)

#' @export
#' @rdname time
#' @aliases time,TimeLine-method
setMethod(
  f = "time",
  signature = "TimeLine",
  definition = function(x) {
    methods::as(x, "numeric", strict = TRUE)
  }
)

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod(
  f = "time",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(years(x))
)

#' @export
#' @rdname time
#' @aliases frequency,TimeLine-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) abs(1 / diff(time(x)))
)

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) methods::callGeneric(years(x))
)
