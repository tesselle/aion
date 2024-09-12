# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To RataDie ===================================================================
#' @export
#' @rdname as_fixed
#' @aliases as_fixed,numeric-method
setMethod(
  f = "as_fixed",
  signature = "numeric",
  definition = function(from) .RataDie(from)
)

# To data.frame ================================================================
#' @export
#' @method as.data.frame TimeSeries
as.data.frame.TimeSeries <- function(x, ..., calendar = NULL) {
  ## Build a long data frame
  z <- as.data.frame.table(x, base = list("T", "S", LETTERS))

  ## Add sampling times
  z[[1]] <- time(x, calendar = calendar)

  ## Fix colnames
  colnames(z) <- c("time", "series", "variable", "value")

  z
}

#' @export
#' @describeIn as.data.frame Returns a long [`data.frame`] with the following columns:
#'  \describe{
#'   \item{`time`}{The (decimal) years at which the time series was sampled.}
#'   \item{`series`}{The name of the time series.}
#'   \item{`variable`}{The name of the variables.}
#'   \item{`value`}{The observed value.}
#'  }
#' @aliases as.data.frame,TimeSeries-method
setMethod("as.data.frame", "TimeSeries", as.data.frame.TimeSeries)

#' @export
#' @method as.data.frame TimeIntervals
as.data.frame.TimeIntervals <- function(x, ..., calendar = NULL) {
  ## Build a data frame
  z <- data.frame(
    label = labels(x),
    start = start(x, calendar = calendar),
    end = end(x, calendar = calendar)
  )

  z
}

#' @export
#' @describeIn as.data.frame Returns a [`data.frame`] with the following columns:
#'  \describe{
#'   \item{`label`}{The name of the intervals.}
#'   \item{`start`}{The start time of the intervals, in (decimal) years.}
#'   \item{`end`}{The end time of the intervals, in (decimal) years.}
#'  }
#' @aliases as.data.frame,TimeIntervals-method
setMethod("as.data.frame", "TimeIntervals", as.data.frame.TimeIntervals)
