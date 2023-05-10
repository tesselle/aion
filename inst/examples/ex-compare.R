## Vectors of years
x <- as_fixed(c(-9551, -7917, -8387, -3371, -3004), calendar = CE())
y <- as_fixed(c(-3940, -4055, -8528, -5068, -8442), calendar = CE())

## x precedes y?
x < y

## x preceded by y?
x > y
