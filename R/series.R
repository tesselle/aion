# TIME SERIES
#' @include AllGenerics.R
NULL

# array ========================================================================
#' @export
#' @rdname series
#' @aliases series,array,RataDie,missing-method
setMethod(
  f = "series",
  signature = c(object = "array", time = "RataDie", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    ## Validation
    arkhe::assert_length(time, NROW(object))

    ## Set the names of the series
    n <- dim(object)[2L]
    if (!is.null(names)) {
      arkhe::assert_length(names, n)
      dimnames(object)[[2L]] <- names
    }
    if (is.null(dimnames(object)[[2L]])) {
      dimnames(object)[[2L]] <- paste0("S", seq_len(n))
    }

    ## Chronological order
    i <- order(time, decreasing = FALSE)
    time <- time[i]
    object <- object[i, , , drop = FALSE]

    .TimeSeries(object, .Time = time)
  }
)

#' @export
#' @rdname series
#' @aliases series,array,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "array", time = "numeric", calendar = "TimeScale"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    if (methods::is(time, "RataDie")) {
      msg <- "%s is already expressed in rata die: %s is ignored."
      warning(sprintf(msg, sQuote("time"), sQuote("calendar")), call. = FALSE)
    } else {
      time <- fixed(time, calendar = calendar, scale = scale)
    }
    methods::callGeneric(object = object, time = time, names = names)
  }
)

# matrix =======================================================================
#' @export
#' @rdname series
#' @aliases series,matrix,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "numeric", calendar = "TimeScale"),
  definition = function(object, time, calendar, scale = 1, names = NULL) {
    x <- array(object, dim = c(dim(object), 1))
    rownames(x) <- rownames(object)
    colnames(x) <- colnames(object)
    methods::callGeneric(object = x, time = time, calendar = calendar,
                         scale = scale, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,matrix,RataDie,missing-method
setMethod(
  f = "series",
  signature = c(object = "matrix", time = "RataDie", calendar = "missing"),
  definition = function(object, time, names = NULL) {
    x <- array(object, dim = c(dim(object), 1))
    colnames(x) <- colnames(object)
    methods::callGeneric(object = x, time = time, names = names)
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
    object <- array(data = object, dim = c(length(object), 1, 1))
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
    object <- array(data = object, dim = c(length(object), 1, 1))
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
