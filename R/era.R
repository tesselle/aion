# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname era
#' @aliases era,character-method
setMethod(
  f = "era",
  signature = c(x = "character"),
  definition = function(x) {
    ## Validation
    x <- match.arg(x, names(.era), several.ok = FALSE)
    x <- .era[[x]]
    u <- match.arg(x$unit, names(.unit), several.ok = FALSE)
    u <- .unit[[u]]

    .TimeScale(
      unit = u$unit,
      days = u$length,
      label = x$label,
      name = x$name,
      epoch = x$epoch,
      scale = x$scale,
      direction = x$direction
    )
  }
)

#' @export
#' @rdname convert
#' @aliases convert,TimeScale,TimeScale-method
setMethod(
  f = "convert",
  signature = c(from = "TimeScale", to = "TimeScale"),
  definition = function(from, to) {
    ## Validation
    if (anyNA(from@days) | anyNA(to@days)) {
      msg <- ""
      stop(msg, call. = FALSE)
    }

    fun <- function(x, precision = getOption("chronos.precision")) {
      ## Rescale to 1 (if not already)
      x <- x * from@scale

      ## Transformation
      ux <- from@days
      uy <- to@days
      dx <- from@direction
      dy <- to@direction
      ex <- from@epoch
      ey <- to@epoch
      y <- ((ux * dx * dy * x) + (365.2425 * dy * (ex - ey))) / uy

      ## Apply destination scale
      y <- y / to@scale

      ## Round to precision
      if (!is.na(precision)) {
        y <- round(y / precision) * precision
      }

      return(y)
    }

    return(fun)
  }
)
