# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname era
#' @aliases era,character-method
setMethod(
  f = "era",
  signature = c(object = "character"),
  definition = function(object) {
    ## Validation
    x <- match.arg(object, names(.era), several.ok = FALSE)
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
#' @rdname era
#' @aliases era,CalibratedAges-method
setMethod(
  f = "era",
  signature = c(object = "CalibratedAges"),
  definition = function(object) {
    object@calendar
  }
)

#' @export
#' @rdname era
#' @aliases era,CalibratedSPD-method
setMethod(
  f = "era",
  signature = c(object = "CalibratedSPD"),
  definition = function(object) {
    object@calendar
  }
)

# Grid =========================================================================
#' @export
#' @rdname years
#' @aliases years,CalibratedAges-method
setMethod(
  f = "years",
  signature = "CalibratedAges",
  definition = function(object) {
    direction <- era(object)@direction
    seq(
      from = object@start,
      by = object@resolution * direction,
      length.out = ncol(object)
    )
  }
)

#' @export
#' @rdname years
#' @aliases years,CalibratedSPD-method
setMethod(
  f = "years",
  signature = "CalibratedSPD",
  definition = function(object) {
    direction <- era(object)@direction
    seq(
      from = object@start,
      by = object@resolution * direction,
      length.out = length(object)
    )
  }
)

# Convert ======================================================================
#' @export
#' @rdname convert
#' @aliases convert,character,character-method
setMethod(
  f = "convert",
  signature = c(from = "character", to = "character"),
  definition = function(from, to) {
    methods::callGeneric(from = era(from), to = era(to))
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

# Project ======================================================================
#' @export
#' @rdname project
#' @aliases project,CalibratedAges,character-method
setMethod(
  f = "project",
  signature = c(object = "CalibratedAges", target = "character"),
  definition = function(object, target) {
    origin <- era(object)
    target <- era(target)
    fun <- convert(origin, target)
    methods::initialize(object, object@.Data, start = fun(object@start), calendar = target)
  }
)

#' @export
#' @rdname project
#' @aliases project,CalibratedSPD,character-method
setMethod(
  f = "project",
  signature = c(object = "CalibratedSPD", target = "character"),
  definition = function(object, target) {
    origin <- era(object)
    target <- era(target)
    fun <- convert(origin, target)
    methods::initialize(object, start = fun(object@start), calendar = target)
  }
)
