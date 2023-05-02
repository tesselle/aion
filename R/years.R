# YEAR VECTORS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname years
#' @aliases years,numeric,Calendar-method
setMethod(
  f = "years",
  signature = c(object = "numeric", calendar = "Calendar"),
  definition = function(object, calendar, scale = 1, sort = FALSE) {
    if (isTRUE(sort)) {
      i <- order(object, decreasing = calendar_direction(calendar) < 0)
      object <- object[i]
    }
    .TimeLine(object, calendar = calendar, scale = as.integer(scale))
  }
)
