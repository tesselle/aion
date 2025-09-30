# GRAPH
#' @include AllGenerics.R
NULL

# S3 class as S4 ===============================================================
setClass("igraph")

# Create =======================================================================
#' @export
#' @rdname graph_create
#' @aliases graph_create,data.frame-method
setMethod(
  f = "graph_create",
  signature = c(object = "data.frame"),
  definition = function(object, type = c("interval", "stratigraphy"),
                        direction = c("above", "below"),
                        verbose = getOption("aion.verbose"), ...) {
    ## Coerce to character matrix
    object[] <- lapply(X = object, FUN = as.character)
    object <- as.matrix(object)

    methods::callGeneric(object, type = type, direction = direction,
                         verbose = verbose, ...)
  }
)

#' @export
#' @rdname graph_create
#' @aliases graph_create,matrix-method
setMethod(
  f = "graph_create",
  signature = c(object = "matrix"),
  definition = function(object, type = c("interval", "stratigraphy"),
                        direction = c("above", "below"),
                        verbose = getOption("aion.verbose"), ...) {
    ## Validation
    arkhe::assert_package("igraph")
    arkhe::assert_type(object, "character")
    arkhe::assert_dim(object, c(nrow(object), 2L))
    type <- match.arg(type, several.ok = FALSE)
    direction <- match.arg(direction, several.ok = FALSE)

    ## Remove singletons
    object <- arkhe::discard_rows(object, f = anyNA, verbose = verbose)

    ## Graph type
    if (identical(type, "interval")) directed <- FALSE
    if (identical(type, "stratigraphy")) directed <- TRUE

    ## Switch relations, if needed
    ## (does not matter if type == "interval")
    if (identical(direction, "below")) {
      object <- object[, c(2, 1)]
    }

    ## Reorder
    object <- object[order(object[, 1], object[, 2]), ]

    ## Create graph
    graph <- igraph::graph_from_edgelist(el = object, directed = directed)

    ## Check
    if (identical(type, "interval")) assert_graph_chordal(graph)
    if (identical(type, "stratigraphy")) assert_graph_dag(graph)

    graph
  }
)

#' @export
#' @rdname graph_create
#' @aliases graph_create,TimeIntervals-method
setMethod(
  f = "graph_create",
  signature = c(object = "TimeIntervals"),
  definition = function(object, type = c("interval", "stratigraphy"),
                        verbose = getOption("aion.verbose"), ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Detect relations
    rel <- switch(
      type,
      interval = rbind(overlaps(object), finishes(object), contains(object),
                       starts(object), equals(object)),
      stratigraphy = rbind(preceded_by(object), met_by(object))
    )

    methods::callGeneric(rel, type = type, direction = "above",
                         verbose = verbose, ...)
  }
)

# Prune ========================================================================
#' @export
#' @rdname graph_prune
#' @aliases graph_prune,igraph-method
setMethod(
  f = "graph_prune",
  signature = c(object = "igraph"),
  definition = function(object, reduce = TRUE,
                        remove_multiple = TRUE, remove_loops = TRUE, ...) {

    ## Transitive reduction
    if (isTRUE(reduce) && isTRUE(igraph::is_dag(object))) {
      arkhe::assert_package("relations")

      edges <- as.matrix(object, matrix.type = c("edgelist"))
      endo <- relations::endorelation(
        # domain = lapply(unique(as.character(edges)), sets::as.set),
        graph = as.data.frame(edges)
      )
      red <- relations::transitive_reduction(endo)
      mat <- relations::relation_incidence(red)
      object <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")
    }

    ## Remove multiple edges and loop edges
    if (isTRUE(remove_multiple) || isTRUE(remove_loops)) {
      object <- igraph::simplify(
        object,
        remove.multiple = remove_multiple,
        remove.loops = remove_loops
      )
    }

    object
  }
)

# Check ========================================================================
## Check if DAG
assert_graph_dag <- function(x, must_fail = FALSE) {
  is_dag <- igraph::is_dag(x)
  if (!isTRUE(is_dag)) {
    msg <- tr_("This is not a stratigraphic graph!")
    do <- if (isTRUE(must_fail)) stop else warning
    do(msg, "\n", tr_("This is not a directed acyclic graph."), call. = FALSE)
  }
  invisible(x)
}

## Check that there are no cycles with more than three nodes
assert_graph_chordal <- function(x, must_fail = FALSE) {
  is_chordal <- igraph::is_chordal(x)$chordal
  if (!isTRUE(is_chordal)) {
    msg <- tr_("This is not an interval graph!")
    do <- if (isTRUE(must_fail)) stop else warning
    do(msg, "\n", tr_("This is not a chordal graph."), call. = FALSE)
  }
  invisible(x)
}
