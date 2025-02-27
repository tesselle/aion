# GRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname as_graph
#' @aliases as_graph,TimeIntervals-method
setMethod(
  f = "as_graph",
  signature = c(object = "TimeIntervals"),
  definition = function(object, aggregate = TRUE, ...) {
    ## Validation
    arkhe::assert_package("igraph")

    ## Compute time overlaps
    int <- overlap(object, calendar = NULL, aggregate = aggregate)

    adj <- int > 0
    graph <- igraph::graph_from_adjacency_matrix(
      adjmatrix = adj,
      mode = "upper",
      diag = FALSE,
      add.colnames = NULL
    )

    ## Check that there are no cycles with more than three edges
    girth <- igraph::girth(graph)$girth
    if (girth > 3) {
      warning(tr_("This is not an interval graph!"), "\n",
              sprintf(tr_("Length of the shortest circle: %g."), girth),
              call. = FALSE)
    }

    graph
  }
)
