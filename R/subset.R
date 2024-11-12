# SUBSET
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,RataDie-method
setMethod(
  f = "[",
  signature = c(x = "RataDie"),
  function(x, i) {
    z <- methods::callNextMethod() # Method for `numeric`
    methods::initialize(x, z)
  }
)

#' @export
#' @rdname subset
#' @aliases [,TimeIntervals-method
setMethod(
  f = "[",
  signature = c(x = "TimeIntervals"),
  function(x, i) {
    id <- x@.Id[i]
    start <- x@.Start[i]
    end <- x@.End[i]
    methods::initialize(x, .Id = id, .Start = start, .End = end)
  }
)

#' @export
#' @rdname subset
#' @aliases [,TimeSeries-method
setMethod(
  f = "[",
  signature = c(x = "TimeSeries"),
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    time <- x@.Time

    z <- z[i, j, k, drop = drop]
    if (isTRUE(drop)) return(z)

    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      time <- time[i]
    }

    methods::initialize(x, z, .Time = time)
  }
)

# Window =======================================================================
#' @export
#' @method window TimeSeries
window.TimeSeries <- function(x, start = NULL, end = NULL, ...) {
  if (is.null(start)) start <- start(x)
  if (is.null(end)) end <- end(x)
  years <- time(x)

  i <- which(years >= start & years <= end)
  x[i, , , drop = FALSE]
}

#' @export
#' @rdname window
#' @aliases window,TimeSeries-method
setMethod("window", "TimeSeries", window.TimeSeries)
