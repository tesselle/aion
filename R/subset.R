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
#' @aliases [,TimeSeries-method
setMethod(
  f = "[",
  signature = c(x = "TimeSeries"),
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    time <- x@time

    z <- z[i, j, k, drop = drop]
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      time <- time[i]
    }

    if (isTRUE(drop)) return(z)
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

    i <- which(years >= start & years <= end)
    x[i, , , drop = FALSE]
  }
)
