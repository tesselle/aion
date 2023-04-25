# SUBSET
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,TimeSeries-method
setMethod(
  f = "[",
  signature = c(x = "TimeSeries"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z))) return(z)

    start <- x@time_start
    labels <- x@time_labels
    if (!missing(i)) {
      years <- time(x)
      start <- years[i][[1L]]
    }
    if (!missing(j)) {
      labels <- labels[j]
    }
    x <- methods::as(x, "TimeSeries", strict = TRUE)
    methods::initialize(x, z, time_labels = labels, time_start = start)
  }
)
