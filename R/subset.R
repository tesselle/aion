# SUBSET
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,TimeLine-method
setMethod(
  f = "[",
  signature = c(x = "TimeLine"),
  function(x, i) {
    z <- methods::callNextMethod() # Method for `numeric`
    methods::initialize(x, z)
  }
)

#' @export
#' @rdname subset
#' @aliases [,TimeSeries-method
setMethod(
  f = "[",
  signature = c(x = "TimeSeries"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod() # Method for `matrix`

    if (is.null(dim(z))) return(z)

    time <- years(x)
    if (!missing(i)) {
      time <- time[i]
    }
    methods::initialize(x, z, time = time)
  }
)

# Window =======================================================================
#' @export
#' @rdname window
#' @aliases window,TimeSeries-method
setMethod(
  f = "window",
  signature = "TimeSeries",
  definition = function(x, start = NULL, end = NULL) {
    if (is.null(start)) start <- start(x)
    if (is.null(end)) end <- end(x)
    years <- time(x)

    if (era_direction(x) > 0) {
      i <- which(years >= start & years <= end)
    }
    if (era_direction(x) < 0) {
      i <- which(years <= start & years >= end)
    }
    x[i, , drop = FALSE]
  }
)
