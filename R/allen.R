# ALLEN INTERVAL ALGEBRA
#' @include AllGenerics.R
NULL

# Codes ========================================================================
#' The Basic Allen Relation Set
#'
#' @param ... Currently not used.
#' @return
#'  * `allen_relation_code()` returns a [`character`] vector of one-letter codes
#'     for the thirteen basic Allen relations.
#'  * `allen_relation_string()` returns a [`character`] vector of string
#'     descriptors of the Allen basic relations.
#'  * `allen_relation_concurrent()` returns a [`character`] vector of nine
#'     one-letter codes for the Allen concurrent relations.
#'  * `allen_relation_distinct()` returns the six value Allen relation set for
#'     intervals with distinct endpoints.
#' @note
#'  The codes were proposed by Thomas Alspaugh.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#'
#'  Alspaugh, T. (2019). Allen's Interval Algebra.
#'  URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
#' @family Allen's intervals
#' @author T. S. Dye, N. Frerebeau
#' @export
allen_relation_code <- function(...) {
  .alspaugh_code
}

#' @rdname allen_relation_code
#' @export
allen_relation_string <- function(...) {
  c(
    `p` = tr_("precedes"),
    `m` = tr_("meets"),
    `o` = tr_("overlaps"),
    `F` = tr_("finished by"),
    `D` = tr_("contains"),
    `s` = tr_("starts"),
    `e` = tr_("equals"),
    `S` = tr_("started by"),
    `d` = tr_("during"),
    `f` = tr_("finishes"),
    `O` = tr_("overlapped by"),
    `M` = tr_("met by"),
    `P` = tr_("preceded by")
  )
}

#' @rdname allen_relation_code
#' @export
allen_relation_concurrent <- function(...) {
  c("o", "F", "s", "D", "e", "S", "d", "f", "O")
}

#' @rdname allen_relation_code
#' @export
allen_relation_distinct <- function(...) {
  c("p", "o", "D", "d", "O", "P")
}

# Relation =====================================================================
#' @export
#' @rdname allen_relation
#' @aliases allen_relation,RataDie,RataDie-method
setMethod(
  f = "allen_relation",
  signature = signature(x = "RataDie", y = "RataDie"),
  definition = function(x, y) {
    ## Validation
    n <- length(x)
    arkhe::assert_length(y, n)
    assert_ordered(x, y)

    comb <- utils::combn(n, m = 2, simplify = TRUE)
    xmin <- x[comb[1, ]]
    xmax <- y[comb[1, ]]
    ymin <- x[comb[2, ]]
    ymax <- y[comb[2, ]]

    mtx <- matrix(data = NA_character_, nrow = n, ncol = n)
    labels <- names(x) %||% names(y)
    rownames(mtx) <- labels
    colnames(mtx) <- labels

    rel <- .relation(xmin, xmax, ymin, ymax)
    mtx[lower.tri(mtx)] <- rel
    mtx <- t(mtx)
    mtx[lower.tri(mtx)] <- allen_converse(rel)
    mtx
  }
)

#' @export
#' @rdname allen_relation
#' @aliases allen_relation,TimeIntervals,missing-method
setMethod(
  f = "allen_relation",
  signature = signature(x = "TimeIntervals", y = "missing"),
  definition = function(x) {
    int_start <- start(x, calendar = NULL)
    int_end <- end(x, calendar = NULL)
    names(int_start) <- labels(x)
    methods::callGeneric(x = int_start, y = int_end)
  }
)

# Complement ===================================================================
#' @export
#' @rdname allen_complement
#' @aliases allen_complement,character-method
setMethod(
  f = "allen_complement",
  signature = signature(x = "character"),
  definition = function(x) {
    code <- paste0(.alspaugh_code, collapse = "")
    vapply(X = x, FUN = sub, FUN.VALUE = character(1), replacement = "",
           x = code, ignore.case = FALSE, USE.NAMES = FALSE)
  }
)

#' @export
#' @rdname allen_complement
#' @aliases allen_complement,matrix-method
setMethod(
  f = "allen_complement",
  signature = signature(x = "matrix"),
  definition = function(x) {
    mp <- dim(x)
    dim(x) <- NULL
    comp <- methods::callGeneric(x)
    dim(comp) <- mp
    comp
  }
)

# Converse =====================================================================
#' @export
#' @rdname allen_converse
#' @aliases allen_converse,character-method
setMethod(
  f = "allen_converse",
  signature = signature(x = "character"),
  definition = function(x) {
    old <- paste0(.alspaugh_code, collapse = "")
    new <- paste0(.alspaugh_converse, collapse = "")
    converse <- chartr(old = old, new = new, x = x)

    # Keep Alspaugh order
    x <- vapply(
      X = strsplit(converse, split = ""),
      FUN = function(x) {
        x <- x[order(match(x, .alspaugh_code))]
        paste0(x, collapse = "")
      },
      FUN.VALUE = character(1)
    )

    x[is.na(converse)] <- NA_character_
    x
  }
)

#' @export
#' @rdname allen_converse
#' @aliases allen_converse,matrix-method
setMethod(
  f = "allen_converse",
  signature = signature(x = "matrix"),
  definition = function(x) {
    mp <- dim(x)
    dim(x) <- NULL
    conv <- methods::callGeneric(x)
    dim(conv) <- mp
    conv
  }
)

# Composition ==================================================================
#' @export
#' @rdname allen_composition
#' @aliases allen_composition,character,character-method
setMethod(
  f = "allen_composition",
  signature = signature(x = "character", y = "character"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    mapply(
      FUN = function(x, y) {
        z <- expand.grid(x, y, stringsAsFactors = FALSE)
        comp <- unlist(.composition[z[, 1], z[, 2]], recursive = TRUE)
        code <- unlist(strsplit(comp, ""), recursive = TRUE)
        code <- code[!duplicated(code)]
        code <- code[order(match(code, .alspaugh_code))] # Keep Alspaugh order
        paste0(code, collapse = "")
      },
      x = strsplit(x, split = ""),
      y = strsplit(y, split = "")
    )

  }
)

# Intersect ====================================================================
#' @export
#' @rdname allen_intersect
#' @aliases allen_intersect,character,character-method
setMethod(
  f = "allen_intersect",
  signature = signature(x = "character", y = "character"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    code <- strsplit(paste0(x, y), split = "")
    vapply(
      X = code,
      FUN = function(x) {
        x <- unique(x[duplicated(x)])
        x <- x[order(match(x, .alspaugh_code))] # Keep Alspaugh order
        paste0(x, collapse = "")
      },
      FUN.VALUE = character(1)
    )
  }
)

# Union ========================================================================
#' @export
#' @rdname allen_union
#' @aliases allen_union,character,character-method
setMethod(
  f = "allen_union",
  signature = signature(x = "character", y = "character"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    code <- strsplit(paste0(x, y), split = "")
    vapply(
      X = code,
      FUN = function(x) {
        x <- x[!duplicated(x)]
        x <- x[order(match(x, .alspaugh_code))] # Keep Alspaugh order
        paste0(x, collapse = "")
      },
      FUN.VALUE = character(1)
    )
  }
)

# Helpers ======================================================================
.relation <- function(xmin, xmax, ymin, ymax) {
  n <- length(xmin)

  ## Basic relations
  p <- .precedes(xmin, xmax, ymin, ymax)      # X precedes Y
  m <- .meets(xmin, xmax, ymin, ymax)         # X meets Y
  o <- .overlaps(xmin, xmax, ymin, ymax)      # X overlaps Y
  fi <- .finished_by(xmin, xmax, ymin, ymax)  # X finished by Y
  D <- .contains(xmin, xmax, ymin, ymax)      # X contains Y

  s <- .starts(xmin, xmax, ymin, ymax)        # X starts Y
  e <- .equals(xmin, xmax, ymin, ymax)        # X equals Y
  S <- .started_by(xmin, xmax, ymin, ymax)    # X started by Y

  d <- .during(xmin, xmax, ymin, ymax)        # X during Y
  f <- .finishes(xmin, xmax, ymin, ymax)      # X finished Y
  O <- .overlapped_by(xmin, xmax, ymin, ymax) # X overlapped by Y

  M <- .met_by(xmin, xmax, ymin, ymax)        # X met by Y
  P <- .preceded_by(xmin, xmax, ymin, ymax)   # X preceded by Y

  ## Merge relations
  relations <- character(n)
  relations[p] <- "p"
  relations[m] <- "m"
  relations[o] <- "o"
  relations[fi] <- "F"
  relations[D] <- "D"
  relations[s] <- "s"
  relations[e] <- "e"
  relations[S] <- "S"
  relations[d] <- "d"
  relations[f] <- "f"
  relations[O] <- "O"
  relations[M] <- "M"
  relations[P] <- "P"

  # factor(relations, levels = .alspaugh_code)
  relations
}

.precedes <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax < ymin
}
.meets <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax == ymin
}
.overlaps <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax > ymin & xmax < ymax
}
.finished_by <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax == ymax
}
.contains <- function(xmin, xmax, ymin, ymax) {
  xmin < ymin & xmax > ymax
}
.starts <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax < ymax
}
.equals <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax == ymax
}
.started_by <- function(xmin, xmax, ymin, ymax) {
  xmin == ymin & xmax > ymax
}
.during <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmax < ymax
}
.finishes <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmax == ymax
}
.overlapped_by <- function(xmin, xmax, ymin, ymax) {
  xmin > ymin & xmin < ymax & xmax > ymax
}
.met_by <- function(xmin, xmax, ymin, ymax) {
  xmin == ymax
}
.preceded_by <- function(xmin, xmax, ymin, ymax) {
  xmin > ymax
}
