## R foundation
(y <- as_fixed(year = 1994, calendar = calendar("CE")))
as_year(y, calendar = calendar("CE"))

## Vector of years expressed in ka BP
(ka <- as_fixed(c(30, 35, 40), calendar = calendar("BP"), scale = 1000))
as_year(ka, calendar = calendar("CE"))
