# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HPDI =========================================================================
#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,numeric,numeric-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.954) {
    ## Compute density
    y <- y / sum(y)

    ## Order the sample (faster sorting with radix method)
    sorted <- sort(y, decreasing = TRUE, method = "radix")
    i <- min(which(cumsum(sorted) >= sum(y) * level))
    h <- sorted[[i]]
    idx <- which(y >= h)

    gap <- which(diff(idx) > 1)
    inf <- idx[c(1, gap + 1)]
    sup <- idx[c(gap, length(idx))]

    int <- mapply(FUN = seq, from = inf, to = sup,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    p <- vapply(X = int, FUN = function(i, y) { sum(y[i]) },
                FUN.VALUE = numeric(1), y = y)

    cbind(start = x[inf], end = x[sup], p = round(p, digits = 2))
  }
)

#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,numeric,missing-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "numeric", y = "missing"),
  definition = function(x, level = 0.954, ...) {
    ## Compute density
    d <- stats::density(x, ...)
    methods::callGeneric(x = d$x, y = d$y, level = level)
  }
)
