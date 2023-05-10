# TIME SERIES
#' @include AllGenerics.R
NULL

# TODO: si calendar est founir avec time -> mauvais dispatch!
# Vérifier la classe de time

# matrix =======================================================================
#' @export
#' @rdname series
#' @aliases series,matrix,RataDie,missing-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "RataDie", calendar = "missing"),
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
#' @aliases series,matrix,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "numeric", calendar = "TimeScale"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    time <- as_fixed(time, calendar = calendar, scale = scale)
    methods::callGeneric(object = object, time = time, names = names)
  }
)

# numeric ======================================================================
#' @export
#' @rdname series
#' @aliases series,numeric,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "numeric", time = "numeric", calendar = "TimeScale"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    object <- matrix(data = object, ncol = 1)
    methods::callGeneric(object = object, time = time, calendar = calendar,
                         scale = scale, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,numeric,RataDie,missing-method
setMethod(
  f = "series",
  signature = c(object = "numeric", time = "RataDie", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    object <- matrix(data = object, ncol = 1)
    methods::callGeneric(object = object, time = time, names = names)
  }
)

# data.frame ===================================================================
#' @export
#' @rdname series
#' @aliases series,data.frame,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "data.frame", time = "numeric", calendar = "TimeScale"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object = object, time = time, calendar = calendar,
                         scale = scale, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,data.frame,RataDie,missing-method
setMethod(
  f = "series",
  signature = c(object = "data.frame", time = "RataDie", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object = object, time = time, names = names)
  }
)
