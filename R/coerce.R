# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame TimeSeries
as.data.frame.TimeSeries <- function(x, ..., calendar = NULL) {
  data.frame(time = time(x, calendar = calendar), x@.Data, ...)
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,TimeSeries-method
setMethod("as.data.frame", "TimeSeries", as.data.frame.TimeSeries)
