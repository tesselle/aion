# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To RataDie ===================================================================
#' @export
#' @rdname as_fixed
#' @aliases as_fixed,numeric-method
setMethod(
  f = "as_fixed",
  signature = "numeric",
  definition = function(from) .RataDie(from)
)

# To data.frame ================================================================
#' @export
#' @method as.data.frame TimeSeries
as.data.frame.TimeSeries <- function(x, ..., calendar = NULL) {
  ## Build a long data frame
  z <- as.data.frame.table(x, base = list("T", "S", LETTERS))

  ## Add sampling times
  z[[1]] <- time(x, calendar = calendar)

  ## Fix colnames
  colnames(z) <- c("time", "series", "variable", "value")

  z
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,TimeSeries-method
setMethod("as.data.frame", "TimeSeries", as.data.frame.TimeSeries)
