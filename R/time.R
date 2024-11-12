# TIME
#' @include AllGenerics.R
NULL

#' @export
#' @method start TimeSeries
start.TimeSeries <- function(x, calendar = NULL, ...) {
  z <- min(x@.Time)
  if (is.null(calendar)) return(z)
  as_year(z, calendar = calendar, decimal = TRUE)
}

#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod("start", "TimeSeries", start.TimeSeries)

#' @export
#' @method start TimeIntervals
start.TimeIntervals <- function(x, calendar = NULL, ...) {
  z <- x@.Start
  if (is.null(calendar)) return(z)
  as_year(z, calendar = calendar, decimal = TRUE)
}

#' @export
#' @rdname start
#' @aliases start,TimeIntervals-method
setMethod("start", "TimeIntervals", start.TimeIntervals)

#' @export
#' @method end TimeSeries
end.TimeSeries <- function(x, calendar = NULL, ...) {
  z <- max(x@.Time)
  if (is.null(calendar)) return(z)
  as_year(z, calendar = calendar, decimal = TRUE)
}

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod("end", "TimeSeries", end.TimeSeries)

#' @export
#' @method end TimeIntervals
end.TimeIntervals <- function(x, calendar = NULL, ...) {
  z <- x@.End
  if (is.null(calendar)) return(z)
  as_year(z, calendar = calendar, decimal = TRUE)
}

#' @export
#' @rdname start
#' @aliases end,TimeIntervals-method
setMethod("end", "TimeIntervals", end.TimeIntervals)

#' @export
#' @method time TimeSeries
time.TimeSeries <- function(x, calendar = NULL, ...) {
  z <- x@.Time
  if (is.null(calendar)) return(z)
  as_year(z, calendar = calendar, decimal = TRUE)
}

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod("time", "TimeSeries", time.TimeSeries)

#' @export
#' @method frequency TimeSeries
frequency.TimeSeries <- function(x, ...) {
  mean(abs(1 / diff(time(x))))
}

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod("frequency", "TimeSeries", frequency.TimeSeries)
