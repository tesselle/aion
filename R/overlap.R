# OVERLAP
#' @include AllGenerics.R
NULL

#' @export
#' @rdname overlap
#' @aliases overlap,TimeIntervals-method
setMethod(
  f = "overlap",
  signature = "TimeIntervals",
  definition = function(x, calendar = NULL, aggregate = TRUE) {
    labels <- labels(x)
    lower <- start(x, calendar = calendar) * calendar_direction(calendar)
    upper <- end(x, calendar = calendar) * calendar_direction(calendar)
    m <- length(x)

    ## Compute overlap
    index <- utils::combn(
      x = seq_len(m),
      m = 2,
      FUN = function(x) max(0, min(upper[x]) - max(lower[x]) + 1)
    )

    ## Matrix of results
    mtx <- matrix(data = upper - lower, nrow = m, ncol = m, dimnames = list(labels, labels))
    mtx[lower.tri(mtx, diag = FALSE)] <- index
    mtx <- t(mtx)
    mtx[lower.tri(mtx, diag = FALSE)] <- index

    ## Aggregate in case of disjoint intervals referring to the same event
    if (isTRUE(aggregate)) {
      mtx <- t(rowsum(mtx, group = rownames(mtx), reorder = FALSE))
      mtx <- rowsum(mtx, group = rownames(mtx), reorder = FALSE)
    }

    mtx
  }
)
