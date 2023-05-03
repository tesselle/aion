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
  definition = function(from, to, precision = getOption("chronos.precision")) {
    methods::callGeneric(from = era(from), to = era(to), precision = precision)
  }
)

#' @export
#' @rdname convert
#' @aliases convert,TimeScale,TimeScale-method
setMethod(
  f = "convert",
  signature = c(from = "TimeScale", to = "TimeScale"),
  definition = function(from, to, precision = getOption("chronos.precision")) {
    ## Validation
    if (anyNA(era_year(from)) | anyNA(era_year(to))) {
      stop("Year length is undefined.", call. = FALSE)
    }

    fun <- function(x) {
      ## Transformation
      ux <- era_year(from)
      uy <- era_year(to)
      dx <- era_direction(from)
      dy <- era_direction(to)
      ex <- era_epoch(from)
      ey <- era_epoch(to)
      y <- ((ux * dx * dy * x) + (365.2425 * dy * (ex - ey))) / uy
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
  signature = c(object = "TimeLine", target = "TimeScale"),
  definition = function(object, target) {
    fun <- convert(era(object), target)

    data <- object * object@scale # Rescale to 1 (if not already)
    data <- fun(data)
    error <- fun(object@error)

    methods::initialize(object, data, error = error,
                        scale = 1, calendar = target)
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,character-method
setMethod(
  f = "project",
  signature = c(object = "TimeLine", target = "TimeLine"),
  definition = function(object, target) {
    z <- methods::callGeneric(object, era(target))
    z <- z / target@scale # Apply destination scale

    methods::initialize(target, z)
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,TimeScale-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "TimeSeries"),
  definition = function(object, target) {
    z <- methods::callGeneric(years(object), years(target))

    object@time <- z
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,TimeScale-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "TimeScale"),
  definition = function(object, target) {
    z <- methods::callGeneric(years(object), target)

    object@time <- z
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,TimeScale-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "character"),
  definition = function(object, target) {
    z <- methods::callGeneric(years(object), era(target))

    object@time <- z
    methods::validObject(object)
    object
  }
)
