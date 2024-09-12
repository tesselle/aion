# MUTATORS
#' @include AllGenerics.R
NULL

# Getters ======================================================================
#' @export
#' @method labels TimeSeries
labels.TimeSeries <- function(object, ...) colnames(object)

#' @rdname labels
#' @aliases labels,TimeSeries-method
setMethod("labels", "TimeSeries", labels.TimeSeries)

#' @export
#' @method labels TimeIntervals
labels.TimeIntervals <- function(object, ...) object@.Id

#' @rdname labels
#' @aliases labels,TimeIntervals-method
setMethod("labels", "TimeIntervals", labels.TimeIntervals)

#' @export
#' @method length TimeIntervals
length.TimeIntervals <- function(x) length(x@.Id)

#' @rdname length
#' @aliases length,TimeIntervals-method
setMethod("length", "TimeIntervals", length.TimeIntervals)
