# GRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname graph
#' @aliases graph,matrix-method
setMethod(
  f = "graph",
  signature = c(x = "matrix"),
  definition = function(x, type = c("interval", "stratigraphy"),
                        direction = c("above", "below"),
                        simplify = TRUE, reduce = TRUE, ...) {
    ## Validation
    arkhe::assert_package("igraph")
    arkhe::assert_dim(x, c(nrow(x), 2L))
    arkhe::assert_type(x, "character")
    arkhe::assert_missing(x)
    type <- match.arg(type, several.ok = FALSE)
    direction <- match.arg(direction, several.ok = FALSE)

    ## Graph type
    if (identical(type, "interval")) {
      directed <- FALSE
      mode <- "upper"
    }
    if (identical(type, "stratigraphy")) {
      directed <- TRUE
      mode <- "directed"
      if (identical(direction, "below")) {
        x <- x[, c(2, 1)] # Fix relations, if needed
      }
    }

    ## Reorder
    x <- x[order(x[, 1], x[, 2]), ]

    ## Create graph
    ## Transitive reduction
    if (identical(type, "stratigraphy") && isTRUE(reduce)) {
      arkhe::assert_package("relations")
      endo <- relations::endorelation(
        # domain = lapply(unique(as.character(x)), sets::as.set),
        graph = as.data.frame(x)
      )
      red <- relations::transitive_reduction(endo)
      mat <- relations::relation_incidence(red)
      graph <- igraph::graph_from_adjacency_matrix(mat, mode = mode)
    } else {
      graph <- igraph::graph_from_edgelist(el = x, directed = directed)
    }

    ## Remove multiple edges and loop edges
    if (isTRUE(simplify)) {
      graph <- igraph::simplify(graph, remove.multiple = TRUE,
                                remove.loops = TRUE)
    }

    ## Check
    ## Check that there are no cycles with more than three nodes
    is_chordal <- igraph::is_chordal(graph)$chordal
    if (identical(type, "interval") && !isTRUE(is_chordal)) {
      warn <- tr_("This is not an interval graph!")
      warning(warn, "\n", tr_("This is not a chordal graph."),
              call. = FALSE)
    }

    ## Check if DAG
    is_dag <- igraph::is_dag(graph)
    if (identical(type, "stratigraphy") && !isTRUE(is_dag)) {
      warn <- tr_("This is not a stratigraphic graph!")
      warning(warn, "\n", tr_("This is not a directed acyclic graph."),
              call. = FALSE)
      # return(igraph::feedback_arc_set(graph))
    }

    graph
  }
)

#' @export
#' @rdname graph
#' @aliases graph,TimeIntervals-method
setMethod(
  f = "graph",
  signature = c(x = "TimeIntervals"),
  definition = function(x, type = c("interval", "stratigraphy"),
                        simplify = TRUE, reduce = TRUE, ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Detect relations
    rel <- switch(
      type,
      interval = rbind(overlaps(x), finishes(x), contains(x), starts(x), equals(x)),
      stratigraphy = rbind(preceded_by(x), met_by(x))
    )

    methods::callGeneric(rel, type = type, direction = "above",
                         simplify = simplify, reduce = reduce, ...)
  }
)
