## /!\ Be carefull: chronological comparison (not numerical) /!\

## Vectors of years
(x_CE <- years(c(-9551, -7917, -8387, -3371, -3004), era("CE")))
(y_CE <- years(c(-3940, -4055, -8528, -5068, -8442), era("CE")))

## x precedes y?
x_CE < y_CE

## x preceded by y?
x_CE > y_CE

## Change the origin
(x_BP <- project(x_CE, era("BP")))
(y_BP <- project(y_CE, era("BP")))

## x precedes y?
x_BP < y_BP

all.equal(x_CE < y_CE, x_BP < y_BP)
