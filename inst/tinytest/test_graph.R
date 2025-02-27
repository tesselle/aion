Sys.setenv(LANGUAGE = "en") # Force locale

if (at_home() && requireNamespace("igraph", quietly = TRUE)) {
  int <- intervals(
    start = c(1, 2, 3, 6, 9, 13, 17),
    end = c(7, 4, 15, 14, 11, 18, 19),
    calendar = CE(),
    names = c("A", "B", "C", "D", "E", "F", "G")
  )

  graph <- as_graph(int)
  expect_equal(
    igraph::as_ids(igraph::E(graph)),
    c("A|B", "A|C", "A|D", "B|C", "C|D", "C|E", "C|F", "D|E", "D|F", "F|G")
  )
  expect_equal(
    igraph::as_ids(igraph::V(graph)),
    c("A", "B", "C", "D", "E", "F", "G")
  )
}
