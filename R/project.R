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
    methods::callGeneric(from = calendar(from), to = calendar(to), precision = precision)
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
    if (anyNA(calendar_year(from)) | anyNA(calendar_year(to))) {
      stop("Year length is undefined.", call. = FALSE)
    }

    fun <- function(x) {
      ## Transformation
      ux <- calendar_year(from)
      uy <- calendar_year(to)
      dx <- calendar_direction(from)
      dy <- calendar_direction(to)
      ex <- calendar_epoch(from)
      ey <- calendar_epoch(to)
      y <- ((ux * dx * dy * x) + (365.2425 * dy * (ex - ey))) / uy
      return(y)
    }

    return(fun)
  }
)

# Project ======================================================================
#' @export
#' @rdname project
#' @aliases project,TimeLine,TimeScale-method
setMethod(
  f = "project",
  signature = c(object = "TimeLine", target = "TimeScale"),
  definition = function(object, target) {
    fun <- convert(calendar(object), target)
    x <- fun(time(object))
    methods::initialize(object, x, calendar = target)
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,character-method
setMethod(
  f = "project",
  signature = c(object = "TimeLine", target = "TimeLine"),
  definition = function(object, target) {
    z <- methods::callGeneric(object, calendar(target))
    methods::initialize(target, z)
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
#' @aliases project,TimeSeries,TimeLine-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "TimeLine"),
  definition = function(object, target) {
    z <- methods::callGeneric(years(object), calendar(target))

    object@time <- z
    methods::validObject(object)
    object
  }
)


#' @export
#' @rdname project
#' @aliases project,TimeSeries,TimeSeries-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "TimeSeries"),
  definition = function(object, target) {
    z <- methods::callGeneric(years(object), calendar(target))

    object@time <- z
    methods::validObject(object)
    object
  }
)
