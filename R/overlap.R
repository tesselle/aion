# OVERLAP
#' @include AllGenerics.R
NULL

#' @export
#' @rdname overlap
#' @aliases overlap,TimeIntervals-method
setMethod(
  f = "overlap",
  signature = "TimeIntervals",
  definition = function(x, calendar = NULL) {
    labels <- labels(x)
    lower <- start(x, calendar = calendar) * calendar_direction(calendar)
    upper <- end(x, calendar = calendar) * calendar_direction(calendar)
    m <- length(x)

    ## Compute overlap
    cbn <- utils::combn(seq_len(m), 2)
    index <- apply(
      X = cbn,
      MARGIN = 2,
      FUN = function(x) max(0, min(upper[x]) - max(lower[x]) + 1)
    )

    ## Matrix of results
    mtx <- matrix(data = upper - lower, nrow = m, ncol = m, dimnames = list(labels, labels))
    mtx[lower.tri(mtx, diag = FALSE)] <- index
    mtx <- t(mtx)
    mtx[lower.tri(mtx, diag = FALSE)] <- index

    mtx
  }
)
