if (requireNamespace("igraph", quietly = TRUE)) {
  ## Seven intervals
  int <- intervals(
    start = c(1, 2, 3, 6, 9, 13, 17),
    end = c(7, 4, 15, 14, 11, 18, 19),
    calendar = CE(),
    names = c("A", "B", "C", "D", "E", "F", "G")
  )

  ## Do the intervals overlap?
  overlap(int) > 0

  ## Interval graph
  g <- graph(int)
  plot(g)
}
