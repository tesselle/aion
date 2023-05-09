## R foundation
(y <- years(c(1994, 2000), calendar("CE")))
format(y)

## Vector of years expressed in ka BP
(ka <- years(c(30, 35, 40), calendar("BP"), scale = 1000))
format(ka)
format(ka, format = "ka")
