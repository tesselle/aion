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
    z <- methods::callNextMethod() # Method for `array`

    if (isTRUE(drop)) return(z)
    if (is.null(dim(z))) z <- array(z, dim = c(length(z), 1, 1))
    if (length(dim(z)) == 2) {
      n <- dim(z)[1L]
      m <- if (!missing(j)) length(j) else dim(x)[2L]
      p <- if (!missing(k)) length(k) else dim(x)[3L]
      z <- array(z, dim = c(n, m, p))
    }

    time <- x@time
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[[1L]])
      if (!is.null(dimnames(x)[[1L]])) dimnames(z)[[1L]] <- dimnames(x)[[1L]][i]
      time <- time[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, dimnames(x)[[2L]])
      if (!is.null(dimnames(x)[[2L]])) dimnames(z)[[2L]] <- dimnames(x)[[2L]][j]
    }
    if (!missing(k)) {
      if (is.character(k)) k <- match(k, dimnames(x)[[3L]])
      if (!is.null(dimnames(x)[[3L]])) dimnames(z)[[3L]] <- dimnames(x)[[3L]][k]
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
    x[i, , , drop = FALSE]
  }
)
