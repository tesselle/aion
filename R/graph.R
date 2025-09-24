# GRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname graph
#' @aliases graph,TimeIntervals-method
setMethod(
  f = "graph",
  signature = c(x = "TimeIntervals"),
  definition = function(x, aggregate = TRUE, ...) {
    ## Validation
    arkhe::assert_package("igraph")

    ## Compute time overlaps
    int <- overlap(x, calendar = NULL, aggregate = aggregate)

    adj <- int > 0
    graph <- igraph::graph_from_adjacency_matrix(
      adjmatrix = adj,
      mode = "upper",
      diag = FALSE,
      add.colnames = NULL
    )

    ## Check that there are no cycles with more than three nodes
    if (!igraph::is_chordal(graph)$chordal) {
      warning(tr_("This is not an interval graph!"), "\n",
              tr_("The graph is not chordal."),
              call. = FALSE)
    }

    graph
  }
)
