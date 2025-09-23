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
#' @method names TimeIntervals
names.TimeIntervals <- function(x) x@.Id

#' @rdname names
#' @aliases names,TimeIntervals-method
setMethod("names", "TimeIntervals", names.TimeIntervals)

#' @export
#' @method length TimeIntervals
length.TimeIntervals <- function(x) length(x@.Id)

#' @rdname length
#' @aliases length,TimeIntervals-method
setMethod("length", "TimeIntervals", length.TimeIntervals)

# Setters ======================================================================
#' @export
#' @method `names<-` TimeIntervals
`names<-.TimeIntervals` <- function(x, value) {
  if (is.null(value)) value <- paste0("I", seq_len(length(x)))
  x@.Id <- as.character(value)
  validObject(x)
  x
}

#' @rdname names
#' @aliases names<-,TimeIntervals-method
setMethod("names<-", "TimeIntervals", `names<-.TimeIntervals`)
