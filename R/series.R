# TIME SERIES
#' @include AllGenerics.R
NULL

# matrix =======================================================================
#' @export
#' @rdname series
#' @aliases series,matrix,TimeLine,missing-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "TimeLine", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    ## Set the names of the series
    if (!is.null(names))
      colnames(object) <- names
    if (is.null(colnames(object)))
      colnames(object) <- paste0("S", seq_len(ncol(object)))

    .TimeSeries(object, time = time)
  }
)

#' @export
#' @rdname series
#' @aliases series,matrix,numeric,Calendar-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "numeric", calendar = "Calendar"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    time <- years(time, calendar = calendar, scale = scale)
    methods::callGeneric(object = object, time = time, names = names)
  }
)

# numeric ======================================================================
#' @export
#' @rdname series
#' @aliases series,numeric,numeric,Calendar-method
setMethod(
  f = "series",
  signature = c(object = "numeric", time = "numeric", calendar = "Calendar"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    object <- matrix(data = object, ncol = 1)
    methods::callGeneric(object = object, time = time, calendar = calendar,
                         scale = scale, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,numeric,TimeLine,missing-method
setMethod(
  f = "series",
  signature = c(object = "numeric", time = "TimeLine", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    object <- matrix(data = object, ncol = 1)
    methods::callGeneric(object = object, time = time, names = names)
  }
)

# data.frame ===================================================================
#' @export
#' @rdname series
#' @aliases series,data.frame,numeric,Calendar-method
setMethod(
  f = "series",
  signature = c(object = "data.frame", time = "numeric", calendar = "Calendar"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object = object, time = time, calendar = calendar,
                         scale = scale, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,data.frame,TimeLine,missing-method
setMethod(
  f = "series",
  signature = c(object = "data.frame", time = "TimeLine", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object = object, time = time, names = names)
  }
)
