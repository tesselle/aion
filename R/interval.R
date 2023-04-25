# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# HPDI =========================================================================
#' @export
#' @rdname hpdi
#' @aliases hpdi,numeric,numeric-method
setMethod(
  f = "hpdi",
  signature = c(object = "numeric", density = "numeric"),
  definition = function(object, density, level = 0.954) {
    ## Compute density
    x <- object
    y <- density / sum(density)

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

    cbind(start = x[inf], stop = x[sup], p = round(p, digits = 2))
  }
)

#' @export
#' @rdname hpdi
#' @aliases hpdi,CalibratedAges,missing-method
setMethod(
  f = "hpdi",
  signature = c(object = "CalibratedAges", density = "missing"),
  definition = function(object, level = 0.954) {
    hpd <- apply(
      X = object,
      MARGIN = 2,
      FUN = function(x, years, level) hpdi(years, density = x, level = level),
      years = time(object),
      level = level,
      simplify = FALSE
    )

    names(hpd) <- names(object)
    hpd
  }
)
