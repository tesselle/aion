# REPROJECT
#' @include AllGenerics.R
NULL

# Convert ======================================================================
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
#' @aliases convert,Calendar,Calendar-method
setMethod(
  f = "convert",
  signature = c(from = "Calendar", to = "Calendar"),
  definition = function(from, to) {
    ## Validation
    if (anyNA(calendar_year(from)) | anyNA(calendar_year(to))) {
      stop("Year length is undefined.", call. = FALSE)
    }

    fun <- function(x, precision = getOption("chronos.precision")) {
      ## FIXME: rescale to 1 (if not already)

      ## Transformation
      ux <- calendar_year(from)
      uy <- calendar_year(to)
      dx <- calendar_direction(from)
      dy <- calendar_direction(to)
      ex <- calendar_epoch(from)
      ey <- calendar_epoch(to)
      y <- ((ux * dx * dy * x) + (365.2425 * dy * (ex - ey))) / uy

      ## FIXME: apply destination scale

      ## Round to precision
      if (!is.na(precision)) {
        y <- round(y / precision) * precision
      }

      return(y)
    }

    return(fun)
  }
)

# Project ======================================================================
#' @export
#' @rdname project
#' @aliases project,TimeSeries,character-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "character"),
  definition = function(object, target) {
    target <- calendar(target)
    methods::callGeneric(object, target)
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,Calendar-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "Calendar"),
  definition = function(object, target) {
    ## Drop subclasses, if any
    time_series <- methods::as(object, "TimeSeries", strict = TRUE)
    time_scale <- methods::as(target, "Calendar", strict = TRUE)

    ## Preserve class inheritance (?)
    fun <- convert(time_series, time_scale)
    methods::initialize(object, time = fun(time(object)))
  }
)
