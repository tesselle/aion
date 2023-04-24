# ACCESSORS
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,CalibratedAges-method
setMethod(
  f = "[",
  signature = c(x = "CalibratedAges"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z))) {
      return(z)
    }

    labels <- x@labels
    ages <- x@ages
    errors <- x@errors
    curves <- x@curves
    status <- x@status
    if (!missing(i)) {
      labels <- labels[i]
      ages <- ages[i]
      errors <- errors[i]
      curves <- curves[i]
      status <- status[i]
    }
    methods::initialize(x, z, labels = labels, ages = ages, errors = errors,
                        curves = curves, status = status)
  }
)
