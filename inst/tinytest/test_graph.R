Sys.setenv(LANGUAGE = "en") # Force locale

if (requireNamespace("igraph", quietly = TRUE)) {
  ## Interval graph
  int <- intervals(
    start = c(1, 2, 3, 6, 9, 13, 17),
    end = c(7, 4, 15, 14, 11, 18, 19),
    calendar = CE(),
    names = c("A", "B", "C", "D", "E", "F", "G")
  )

  graph <- graph(int, type = "interval")
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
    graph <- graph(int, type = "stratigraphy", reduce = TRUE)
    expect_equal(
      igraph::as_ids(igraph::E(graph)),
      c("D|B", "E|A", "E|B", "F|E", "G|C", "G|D", "G|E")
    )
    expect_equal(
      igraph::as_ids(igraph::V(graph)),
      c("A", "B", "C", "D", "E", "F", "G")
    )

    ## Direction of relations
    above <- matrix(
      data = c("D", "E", "E", "F", "F", "F", "G", "G", "G", "G", "G",
               "B", "A", "B", "A", "B", "E", "A", "B", "C", "D", "E"),
      ncol = 2
    )
    below <- above[, c(2, 1)]

    graph_above <- graph(above, type = "stratigraphy", direction = "above", reduce = TRUE)
    graph_below <- graph(below, type = "stratigraphy", direction = "below", reduce = TRUE)
    expect_equivalent(
      igraph::as_ids(igraph::E(graph_above)),
      igraph::as_ids(igraph::E(graph_below))
    )
  }

  # https://www.polymtl.ca/pub/sites/lagrapheur/docs/en/documents/NotesChap3.pdf
  # thu <- matrix(
  #   data = c("T", "E", "T", "S", "T", "L", "E", "T", "E", "S",
  #            "S", "T", "S", "E", "S", "L", "S", "G", "S", "M",
  #            "L", "T", "L", "S", "L", "M", "G", "S", "G", "M",
  #            "M", "S", "M", "L", "M", "G"),
  #   ncol = 2, byrow = TRUE
  # )
  # graph <- graph(thu, type = "interval", simplify = TRUE)
  # plot(graph)

  fri <- matrix(
    data = c("T", "B", "T", "L", "T", "G", "B", "T", "B", "S", "B",
             "G", "E", "S", "E", "L", "E", "G", "S", "B", "S", "E",
             "S", "L", "S", "G", "L", "T", "L", "E", "L", "S", "G",
             "T", "G", "B", "G", "E", "G", "S"),
    ncol = 2, byrow = TRUE
  )
  expect_warning(
    graph(fri, type = "interval", simplify = TRUE),
    "not an interval graph"
  )
}
