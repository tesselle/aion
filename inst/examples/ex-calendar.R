## Define time scales
(BP <- calendar("BP"))
(AD <- calendar("AD"))

## Create a custom gregorian calendar
as_gregorian(
  label = "AUC",
  name = "Ab urbe condita",
  epoch = 753,
  direction = 1
)
