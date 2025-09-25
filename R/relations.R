# TEMPORAL RELATIONS
#' @include AllGenerics.R
NULL

# Precede ======================================================================
#' @export
#' @rdname relations
#' @aliases precedes,TimeIntervals-method
setMethod(
  f = "precedes",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .precedes,
      labels = labels(x),
      ...
    )
  }
)

.precedes <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax < ymin
}

#' @export
#' @rdname relations
#' @aliases preceded_by,TimeIntervals,missing-method
setMethod(
  f = "preceded_by",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .preceded_by,
      labels = labels(x),
      ...
    )
  }
)

.preceded_by <- function(xmin, xmax, ymin, ymax) {
  xmin > ymax
}

# Meet =========================================================================
#' @export
#' @rdname relations
#' @aliases meets,TimeIntervals-method
setMethod(
  f = "meets",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .meets,
      labels = labels(x),
      ...
    )
  }
)

.meets <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax == ymin
}

#' @export
#' @rdname relations
#' @aliases met_by,TimeIntervals,missing-method
setMethod(
  f = "met_by",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .met_by,
      labels = labels(x),
      ...
    )
  }
)

.met_by <- function(xmin, xmax, ymin, ymax) {
  xmin == ymax
}

# Overlap ======================================================================
#' @export
#' @rdname relations
#' @aliases overlaps,TimeIntervals-method
setMethod(
  f = "overlaps",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .overlaps,
      labels = labels(x),
      ...
    )
  }
)

.overlaps <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax > ymin & xmax < ymax
}

#' @export
#' @rdname relations
#' @aliases overlapped_by,TimeIntervals,missing-method
setMethod(
  f = "overlapped_by",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .overlapped_by,
      labels = labels(x),
      ...
    )
  }
)

.overlapped_by <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmin < ymax & xmax > ymax
}

# Finish =======================================================================
#' @export
#' @rdname relations
#' @aliases finishes,TimeIntervals,missing-method
setMethod(
  f = "finishes",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .finishes,
      labels = labels(x),
      ...
    )
  }
)

.finishes <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmax == ymax
}

#' @export
#' @rdname relations
#' @aliases finished_by,TimeIntervals-method
setMethod(
  f = "finished_by",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .finished_by,
      labels = labels(x),
      ...
    )
  }
)

.finished_by <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax == ymax
}

# Contain ======================================================================
#' @export
#' @rdname relations
#' @aliases contains,TimeIntervals-method
setMethod(
  f = "contains",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .contains,
      labels = labels(x),
      ...
    )
  }
)

.contains <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax > ymax
}

#' @export
#' @rdname relations
#' @aliases during,TimeIntervals,missing-method
setMethod(
  f = "during",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .during,
      labels = labels(x),
      ...
    )
  }
)

.during <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmax < ymax
}

# Start ========================================================================
#' @export
#' @rdname relations
#' @aliases starts,TimeIntervals,missing-method
setMethod(
  f = "starts",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .starts,
      labels = labels(x),
      ...
    )
  }
)

.starts <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax < ymax
}

#' @export
#' @rdname relations
#' @aliases started_by,TimeIntervals,missing-method
setMethod(
  f = "started_by",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .started_by,
      labels = labels(x),
      ...
    )
  }
)

.started_by <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax > ymax
}

# Equal ========================================================================
#' @export
#' @rdname relations
#' @aliases equals,TimeIntervals,missing-method
setMethod(
  f = "equals",
  signature = c(x = "TimeIntervals"),
  definition = function(x, ...) {
    .relation(
      x = start(x, calendar = NULL),
      y = end(x, calendar = NULL),
      f = .equals,
      labels = labels(x),
      ...
    )
  }
)

.equals <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax == ymax
}

# Helpers ======================================================================
.relation <- function(x, y, f, labels = NULL, use_names = TRUE) {
  n <- length(x)
  arkhe::assert_function(f)
  arkhe::assert_length(y, n)
  assert_ordered(x, y)

  labels <- labels %||% names(x) %||% names(y)
  if (!isTRUE(use_names)) labels <- seq_along(labels)

  el <- seq_len(n)
  comb <- cbind(
    utils::combn(el, m = 2, simplify = TRUE),
    utils::combn(rev(el), m = 2, simplify = TRUE)
  )
  xmin <- x[comb[1, ]]
  xmax <- y[comb[1, ]]
  ymin <- x[comb[2, ]]
  ymax <- y[comb[2, ]]

  mtx <- matrix(
    data = c(labels[comb[1, ]], labels[comb[2, ]]),
    ncol = 2
  )

  rel <- f(xmin, xmax, ymin, ymax)
  mtx <- mtx[rel, , drop = FALSE]
  mtx <- mtx[order(mtx[, 1], mtx[, 2]), ]
  mtx
}
