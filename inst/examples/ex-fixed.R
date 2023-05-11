## R 1.0.0
(y <- fixed(year = 2000, month = 02, day = 29, calendar = CE()))
as_date(y, calendar = CE())
as_year(y, calendar = CE())

## Vector of years expressed in ka BP
(ka <- fixed(c(30, 35, 40), calendar = BP(), scale = 1000))
as_year(ka, calendar = CE())
