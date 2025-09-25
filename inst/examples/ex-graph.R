if (requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("relations", quietly = TRUE)) {
  ## Seven intervals
  int <- intervals(
    start = c(1, 2, 3, 6, 9, 13, 17),
    end = c(7, 4, 15, 14, 11, 18, 19),
    calendar = CE(),
    names = c("A", "B", "C", "D", "E", "F", "G")
  )

  ## Interval graph
  g <- graph(int, type = "interval")
  plot(g)

  ## Stratigraphic graph
  g <- graph(int, type = "strati")
  plot(g, layout = igraph::layout_with_sugiyama)
}
