## R foundation
(y <- years(c(1994, 2000), era("CE")))
format(y)

## Vector of years expressed in ka BP
(ka <- years(c(30, 35, 40), era("BP"), scale = 1000))
format(ka)
format(ka, format = "ka")
