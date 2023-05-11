# CONVERT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname convert
#' @aliases convert,character,character-method
setMethod(
  f = "convert",
  signature = c(from = "character", to = "character"),
  definition = function(from, to) {
    methods::callGeneric(from = calendar(from), to = calendar(to))
  }
)

#' @export
#' @rdname convert
#' @aliases convert,TimeScale,TimeScale-method
setMethod(
  f = "convert",
  signature = c(from = "TimeScale", to = "TimeScale"),
  definition = function(from, to) {
    ## TODO: validation

    fun <- function(x) {
      a <- fixed(x, month = 01, day = 01, calendar = from)
      b <- as_year(a, calendar = to, decimal = TRUE)
      return(b)
    }

    return(fun)
  }
)
