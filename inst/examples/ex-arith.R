## /!\ Be carefull: computations are relative to the epoch /!\

## Vectors of years
(x_CE <- years(c(-350, 31, 1072, 576, 1130), calendar("CE")))
(y_CE <- years(c(1494, 1645, -869, 1440, 1851), calendar("CE")))

## Move forward in time
x_CE + y_CE

## Move backward in time
x_CE - y_CE

## Change the origin
(x_BP <- project(x_CE, calendar("BP")))
(y_BP <- project(y_CE, calendar("BP")))

## Move forward in time
(z_BP <- x_BP + y_BP)

## Change the origin back to CE
(z_CE <- project(z_BP, calendar("CE")))

all.equal(x_CE + y_CE, z_CE)
