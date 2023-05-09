# YEAR VECTORS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname years
#' @aliases years,numeric,TimeScale-method
setMethod(
  f = "years",
  signature = c(object = "numeric", calendar = "TimeScale"),
  definition = function(object, calendar, scale = 1, sort = FALSE) {
    ## Rescale to years (if not already)
    object <- object * scale

    if (isTRUE(sort)) {
      i <- order(object, decreasing = era_direction(calendar) < 0)
      object <- object[i]
    }
    .TimeLine(object, calendar = calendar)
  }
)

#' @export
#' @rdname years
#' @aliases years,TimeSeries,missing-method
setMethod(
  f = "years",
  signature = c(object = "TimeSeries", calendar = "missing"),
  definition = function(object) object@time
)
