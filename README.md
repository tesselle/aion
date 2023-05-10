
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chronos <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/chronos/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/chronos/actions)
[![codecov](https://codecov.io/gh/tesselle/chronos/branch/main/graph/badge.svg?token=UgoOXsZW86)](https://app.codecov.io/gh/tesselle/chronos)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/chronos/badge/main)](https://www.codefactor.io/repository/github/tesselle/chronos/overview/main)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/chronos"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

Base R ships with a lot of functionality useful for time series, in
particular in the **stats** package: a time-series object can be created
with the `stats::ts()` function. These objects are agnostic: the unit of
time is not relevant, only the sampling frequency matters. Most
archaeological time series are different because they are defined for a
given calendar era and may involve dates far in the past.

**chronos** provides a system of classes and methods to represent and
work with such time-series. Dates are represented as *rata die*
(Reingold and Dershowitz 2018), i.e. the number of days since 01-01-01
(Gregorian), with negative values for earlier dates (if you want to work
with vectors of years, you may be interested in the
[**era**](https://github.com/joeroe/era) package). This system allows to
represent dates independently of any calendar: it makes calculations and
comparisons easier.

Once a time series is created with **chronos**, any calendar can be used
for printing or plotting data (defaults to Gregorian Common Era; see
`vignette("manual")`).

This package supports usual Gregorian era (e.g. Common Era, Before
Present), more calendars will be gradually implemented. Note that
**chronos** uses the astronomical notation for Gregorian years (there
*is* a year 0).

## Installation

You can install the released version of **chronos** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("chronos")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/chronos")
```

## Usage

``` r
## Load package
library(chronos)
```

``` r
## Set seed for reproductibility
set.seed(12345)

## Create 6 time-series of 50 observations
## Sampled every two years starting from 2000 BP
X <- series(
  object = matrix(rnorm(300), nrow = 50, ncol = 6),
  time = seq(from = 2000, by = -2, length.out = 50),
  calendar = calendar("BP")
)

## Plot
plot(X) # Default calendar
```

![](man/figures/README-time-series-1.png)<!-- -->

## Contributing

Please note that the **chronos** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-reingold2018" class="csl-entry">

Reingold, Edward M., and Nachum Dershowitz. 2018. *Calendrical
Calculations: The Ultimate Edition*. 4th ed. Cambridge University Press.
<https://doi.org/10.1017/9781107415058>.

</div>

</div>
