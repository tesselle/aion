# TIME SERIES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname series
#' @aliases series,matrix,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "matrix", scale = "TimeScale"),
  definition = function(object, scale, start, increment, labels = NULL) {
    if (!is.null(labels))
      colnames(object) <- labels
    if (is.null(colnames(object)))
      colnames(object) <- paste0("S", seq_len(ncol(object)))

    .TimeSeries(object, scale, time_labels = colnames(object),
                time_start = start, time_increment = increment)
  }
)

#' @export
#' @rdname series
#' @aliases series,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "numeric", scale = "TimeScale"),
  definition = function(object, scale, start, increment, labels = NULL) {
    object <- matrix(data = object, ncol = 1)
    methods::callGeneric(object, scale, start = start, increment = increment,
                         labels = labels)
  }
)

#' @export
#' @rdname series
#' @aliases series,data.frame,TimeScale-method
setMethod(
  f = "series",
  signature = c(object = "data.frame", scale = "TimeScale"),
  definition = function(object, scale, start, increment, labels = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object, scale, start = start, increment = increment,
                         labels = labels)
  }
)

# Mutators =====================================================================
## Getters ---------------------------------------------------------------------
#' @export
#' @rdname mutators
#' @aliases names,TimeSeries-method
setMethod(
  f = "names",
  signature = "TimeSeries",
  definition = function(x) x@time_labels
)

## Setters ---------------------------------------------------------------------
#' @export
#' @rdname mutators
#' @aliases names<-,TimeSeries-method
setMethod(
  f = "names<-",
  signature = "TimeSeries",
  definition = function(x, value) {
    x@time_labels <- value
    methods::validObject(x)
    x
  }
)

# Show =========================================================================
setMethod(
  f = "show",
  signature = "TimeSeries",
  definition = function(object) {
    n <- ncol(object)
    msg <- "%d time series observed between %g and %g %s."
    msg <- sprintf(msg, n, start(object), end(object), format(object))
    print(msg)
  }
)

# Grid =========================================================================
#' @export
#' @rdname start
#' @aliases start,TimeSeries-method
setMethod(
  f = "start",
  signature = "TimeSeries",
  definition = function(x) x@time_start
)

#' @export
#' @rdname start
#' @aliases end,TimeSeries-method
setMethod(
  f = "end",
  signature = "TimeSeries",
  definition = function(x) {
    start(x) + x@time_increment * era_direction(x) * nrow(x) - 1
  }
)

#' @export
#' @rdname time
#' @aliases time,TimeSeries-method
setMethod(
  f = "time",
  signature = "TimeSeries",
  definition = function(x) {
    seq(
      from = start(x),
      by = x@time_increment * era_direction(x),
      length.out = nrow(x)
    )
  }
)

# Subset =======================================================================
#' @export
#' @rdname window
#' @aliases window,TimeSeries-method
setMethod(
  f = "window",
  signature = "TimeSeries",
  definition = function(x, start = NULL, end = NULL) {
    if (is.null(start)) start <- start(x)
    if (is.null(end)) end <- end(x)
    years <- time(x)

    if (era_direction(x) > 0) {
      i <- which(years >= start & years <= end)
    }
    if (era_direction(x) < 0) {
      i <- which(years <= start & years >= end)
    }
    x[i, , drop = FALSE]
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
    target <- era(target)
    methods::callGeneric(object, target)
  }
)

#' @export
#' @rdname project
#' @aliases project,TimeSeries,TimeScale-method
setMethod(
  f = "project",
  signature = c(object = "TimeSeries", target = "TimeScale"),
  definition = function(object, target) {
    ## Drop subclasses, if any
    time_series <- methods::as(object, "TimeSeries", strict = TRUE)
    time_scale <- methods::as(target, "TimeScale", strict = TRUE)

    fun <- convert(time_series, time_scale)
    series <- methods::initialize(time_series, time_scale, time_start = fun(start(object)))

    ## Preserve class inheritance (?)
    methods::initialize(object, series)
  }
)
