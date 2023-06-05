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
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod() # Method for `matrix`

    if (isTRUE(drop)) return(z)
    if (is.null(dim(z))) z <- matrix(z, ncol = 1)

    time <- x@time
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, rownames(x))
      rownames(z) <- rownames(x)[i]
      time <- time[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, colnames(x))
      colnames(z) <- colnames(x)[j]
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

    i <- which(years >= start & years <= end)
    x[i, , drop = FALSE]
  }
)
