# MUTATORS
#' @include AllGenerics.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases names,TimeSeries-method
setMethod(
  f = "names",
  signature = "TimeSeries",
  definition = function(x) x@time_labels
)

# Setters ======================================================================
#' @export
#' @rdname mutators
#' @aliases names<-,TimeSeries-method
setMethod(
  f = "names<-",
  signature = "TimeSeries",
  definition = function(x, value) {
    x@time_labels <- value
    methods::validObject(x)
    x
  }
)
