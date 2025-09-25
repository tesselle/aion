Sys.setenv(LANGUAGE = "en") # Force locale

int <- intervals(
  start = c(2, 1, 3, 6, 9, 13, 17),
  end = c(4, 7, 15, 14, 11, 18, 19),
  calendar = CE(),
  names = c("A", "B", "C", "D", "E", "F", "G")
)

p <- precedes(int)
p_expected <- matrix(
  c("A", "A", "A", "A", "B", "B", "B", "C", "D", "E", "E",
    "D", "E", "F", "G", "E", "F", "G", "G", "G", "F", "G"),
  ncol = 2
)
expect_identical(p, p_expected)

o <- overlaps(int)
o_expected <- matrix(
  c("A", "B", "B", "C", "D", "F",
    "C", "C", "D", "F", "F", "G"),
  ncol = 2
)
expect_identical(o, o_expected)

D <- contains(int)
D_expected <- matrix(
  c("B", "C", "C", "D",
    "A", "D", "E", "E"),
  ncol = 2
)
expect_identical(D, D_expected)
