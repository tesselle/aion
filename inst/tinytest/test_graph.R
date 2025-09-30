Sys.setenv(LANGUAGE = "en") # Force locale

if (requireNamespace("igraph", quietly = TRUE)) {
  ## Interval graph
  int <- intervals(
    start = c(1, 2, 3, 6, 9, 13, 17),
    end = c(7, 4, 15, 14, 11, 18, 19),
    calendar = CE(),
    names = c("A", "B", "C", "D", "E", "F", "G")
  )

  graph <- graph_create(int, type = "interval")
  expect_equal(
    igraph::as_ids(igraph::E(graph)),
    c("A|B", "A|C", "A|D", "B|C", "C|D", "C|E", "C|F", "D|E", "D|F", "F|G")
  )
  expect_equal(
    igraph::as_ids(igraph::V(graph)),
    c("A", "B", "C", "D", "E", "F", "G")
  )

  if (requireNamespace("relations", quietly = TRUE)) {
    ## Stratigraphic graph
    graph <- graph_create(int, type = "stratigraphy")
    expect_equal(
      igraph::as_ids(igraph::E(graph)),
      c("D|B", "E|A", "E|B", "F|A", "F|B", "F|E", "G|A", "G|B", "G|C", "G|D", "G|E")
    )
    expect_equal(
      igraph::as_ids(igraph::V(graph)),
      c("D", "B", "E", "A", "F", "G", "C")
    )

    graph <- graph_prune(graph)
    expect_equal(
      igraph::as_ids(igraph::E(graph)),
      c("D|B", "E|A", "E|B", "F|E", "G|C", "G|D", "G|E")
    )
    expect_equal(
      igraph::as_ids(igraph::V(graph)),
      c("A", "B", "C", "D", "E", "F", "G")
    )

    ## Direction of relations
    above <- data.frame(
      X = c("D", "E", "E", "F", "F", "F", "G", "G", "G", "G", "G"),
      Y = c("B", "A", "B", "A", "B", "E", "A", "B", "C", "D", "E")
    )
    below <- above[, c(2, 1)]

    graph_above <- graph_create(above, type = "stratigraphy", direction = "above")
    graph_below <- graph_create(below, type = "stratigraphy", direction = "below")
    expect_equivalent(
      igraph::as_ids(igraph::E(graph_above)),
      igraph::as_ids(igraph::E(graph_below))
    )
  }

  # https://www.polymtl.ca/pub/sites/lagrapheur/docs/en/documents/NotesChap3.pdf
  thu <- matrix(
    data = c("T", "E", "T", "S", "T", "L", "E", "T", "E", "S",
             "S", "T", "S", "E", "S", "L", "S", "G", "S", "M",
             "L", "T", "L", "S", "L", "M", "G", "S", "G", "M",
             "M", "S", "M", "L", "M", "G"),
    ncol = 2, byrow = TRUE
  )
  graph <- graph_create(thu, type = "interval")
  expect_warning(aion:::assert_graph_dag(graph), "not a stratigraphic graph")
  expect_error(aion:::assert_graph_dag(graph, must_fail = TRUE), "not a stratigraphic graph")
  # graph <- graph_prune(graph, reduce = FALSE)
  # plot(graph)

  fri <- matrix(
    data = c("T", "B", "T", "L", "T", "G", "B", "T", "B", "S", "B",
             "G", "E", "S", "E", "L", "E", "G", "S", "B", "S", "E",
             "S", "L", "S", "G", "L", "T", "L", "E", "L", "S", "G",
             "T", "G", "B", "G", "E", "G", "S"),
    ncol = 2, byrow = TRUE
  )
  expect_warning(graph_create(fri, type = "interval"), "not an interval graph")
  # graph <- graph_create(fri, type = "interval")
  # graph <- graph_prune(graph)
  # plot(graph)
}
