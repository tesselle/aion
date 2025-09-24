## Seven intervals
int <- intervals(
  start = c(1, 2, 3, 6, 9, 13, 17),
  end = c(7, 4, 15, 14, 11, 18, 19),
  calendar = CE(),
  names = c("A", "B", "C", "D", "E", "F", "G")
)

## Basic relations
allen_relation(int)

## Complement
(comp <- allen_complement("F"))

## Converse
(conv <- allen_converse(comp))

## Composition
allen_composition("oFD", "oFDseS")

## Intersection
allen_intersect("pFsSf", "pmoFD")

# Union
allen_union("pFsSf", "pmoFD")
