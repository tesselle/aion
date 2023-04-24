# STATISTICS
#' @include AllGenerics.R
NULL

#' @export
#' @method median CalibratedAges
median.CalibratedAges <- function(x, na.rm = FALSE, ...) {
  i <- apply(
    X = x,
    MARGIN = 1,
    FUN = function(x, na.rm) {
      if (na.rm) x <- x[!is.na(x)]
      z <- cumsum(x)
      which.min(abs(z - max(z) / 2))
    },
    na.rm = na.rm
  )
  years <- years(x)
  years[i]
}

#' @export
#' @rdname median
#' @aliases median,CalibratedAges,missing-method
setMethod("median", c(x = "CalibratedAges"), median.CalibratedAges)

#' @export
#' @method mean CalibratedAges
mean.CalibratedAges <- function(x, na.rm = FALSE, ...) {
  apply(
    X = x,
    MARGIN = 1,
    FUN = function(w, x, na.rm) {
      if (na.rm) {
        i <- !is.na(w) & !is.na(x)
        x <- x[i]
        w <- w[i]
      }
      stats::weighted.mean(x = x, w = w)
    },
    x = years(x),
    na.rm = na.rm
  )
}

#' @export
#' @rdname mean
#' @aliases mean,CalibratedAges,missing-method
setMethod("mean", c(x = "CalibratedAges"), mean.CalibratedAges)
