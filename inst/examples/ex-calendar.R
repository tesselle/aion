## Define time scales
(BP <- calendar("BP"))
(AD <- calendar("AD"))

## Create a custom gregorian calendar
as_gregorian(
  label = "cal BP",
  name = "Before Present",
  epoch = 1950,
  direction = -1
)
