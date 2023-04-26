# TIME SERIES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname series
#' @aliases series,matrix,TimeScale-method
setMethod(
  f = "series",
  signature = c(data = "matrix", scale = "TimeScale"),
  definition = function(data, scale, start, end = NULL, frequency = 1,
                        delta = NULL, names = NULL) {
    ## Validation
    if (!is.null(end)) {
      years <- seq(
        from = start,
        to = end,
        by = 1 / frequency * era_direction(scale)
      )
      if (length(years) != nrow(data)) {
        old <- frequency
        frequency <- abs(1 / ((end - start) / (nrow(data) - 1)))

        msg <- "Frequency is not consistent with the number of observations"
        msg <- sprintf("%s: %g is used instead of %g.", msg, frequency, old)
        warning(msg, call. = FALSE)
      }
    }

    if (!is.null(delta)) {
      frequency <- 1 / delta
    }

    ## Set the names of the series
    if (!is.null(names))
      colnames(data) <- names
    if (is.null(colnames(data)))
      colnames(data) <- paste0("S", seq_len(ncol(data)))

    .TimeSeries(data, scale, time_labels = colnames(data),
                time_start = start, time_frequency = frequency)
  }
)

#' @export
#' @rdname series
#' @aliases series,numeric,TimeScale-method
setMethod(
  f = "series",
  signature = c(data = "numeric", scale = "TimeScale"),
  definition = function(data, scale, start, end = NULL, frequency = 1,
                        delta = NULL, names = NULL) {
    data <- matrix(data = data, ncol = 1)
    methods::callGeneric(data, scale, start = start, end = end,
                         frequency = frequency, delta = delta, names = names)
  }
)

#' @export
#' @rdname series
#' @aliases series,data.frame,TimeScale-method
setMethod(
  f = "series",
  signature = c(data = "data.frame", scale = "TimeScale"),
  definition = function(data, scale, start, end = NULL, frequency = 1,
                        delta = NULL, names = NULL) {
    data <- data.matrix(data)
    methods::callGeneric(data, scale, start = start, end = end,
                         frequency = frequency, delta = delta, names = names)
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
    cat(msg, sep = "\n")
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
    start(x) + (nrow(x) - 1) / frequency(x) * era_direction(x)
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
      by = 1 / frequency(x) * era_direction(x),
      length.out = nrow(x)
    )
  }
)

#' @export
#' @rdname time
#' @aliases frequency,TimeSeries-method
setMethod(
  f = "frequency",
  signature = "TimeSeries",
  definition = function(x) x@time_frequency
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
