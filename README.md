
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aion <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/aion/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/aion/actions)
[![codecov](https://codecov.io/gh/tesselle/aion/branch/main/graph/badge.svg?token=UgoOXsZW86)](https://app.codecov.io/gh/tesselle/aion)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/aion/badge/main)](https://www.codefactor.io/repository/github/tesselle/aion/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/aion)](https://cran.r-project.org/package=aion)

<a href="https://tesselle.r-universe.dev/aion"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/aion"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=aion"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/aion" alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/web/checks/check_results_aion.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/aion.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=aion"
class="pkgdown-release"><img src="http://cranlogs.r-pkg.org/badges/aion"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8032278.svg)](https://doi.org/10.5281/zenodo.8032278)
<!-- badges: end -->

## Overview

Base R ships with a lot of functionality useful for time series, in
particular in the **stats** package. However, these features are not
adapted to most archaeological time series. These are indeed defined for
a given calendar era, they can involve dates very far in the past and
the sampling of the observation time is (in most cases) not constant.

**aion** provides a system of classes and methods to represent and work
with such time-series. Dates are represented as *rata die* (Reingold and
Dershowitz 2018), i.e. the number of days since 01-01-01 (Gregorian),
with negative values for earlier dates. This allows to represent dates
independently of any calendar: it makes calculations and comparisons
easier.

Once a time series is created with **aion**, any calendar can be used
for printing or plotting data (defaults to Gregorian Common Era; see
`vignette("aion")`).

**aion** does not provide tools for temporal modeling. Instead, it
offers a simple API that can be used by other specialized packages.

    To cite aion in publications use:

      Frerebeau N, Roe J (2023). _aion: Archaeological Time Series_.
      Université Bordeaux Montaigne, Pessac, France.
      doi:10.5281/zenodo.8032278 <https://doi.org/10.5281/zenodo.8032278>,
      R package version 1.0.2, <https://packages.tesselle.org/aion/>.

    Une entrée BibTeX pour les utilisateurs LaTeX est

      @Manual{,
        author = {Nicolas Frerebeau and Joe Roe},
        title = {{aion: Archaeological Time Series}},
        year = {2023},
        organization = {Université Bordeaux Montaigne},
        address = {Pessac, France},
        note = {R package version 1.0.2},
        url = {https://packages.tesselle.org/aion/},
        doi = {10.5281/zenodo.8032278},
      }

    This package is a part of the tesselle project
    <https://www.tesselle.org>.

## Installation

You can install the released version of **aion** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("aion")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/aion")
```

## Usage

``` r
## Load package
library(aion)
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

## Related Works

- [**era**](https://github.com/joeroe/era): provides a consistent
  representation of year-based time scales as a numeric vector with an
  associated era.

## Contributing

Please note that the **aion** project is released with a [Contributor
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
