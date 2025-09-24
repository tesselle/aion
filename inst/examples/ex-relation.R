## Seven intervals
int <- intervals(
  start = c(1, 2, 3, 6, 9, 13, 17),
  end = c(7, 4, 15, 14, 11, 18, 19),
  calendar = CE(),
  names = c("A", "B", "C", "D", "E", "F", "G")
)

## Plot intervals
plot(int)

## Temporal relations
precedes(int) # A precedes E...

overlaps(int) # A overlaps C...

contains(int) # A contains B...
