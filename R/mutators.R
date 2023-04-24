# MUTATORS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname mutators
#' @aliases names,CalibratedAges-method
setMethod(
  f = "names",
  signature = "CalibratedAges",
  definition = function(x) x@labels
)

#' @export
#' @rdname mutators
#' @aliases names<-,CalibratedAges-method
setMethod(
  f = "names<-",
  signature = "CalibratedAges",
  definition = function(x, value) {
    x@labels <- value
    methods::validObject(x)
    x
  }
)
