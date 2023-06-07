---
title: 'aion: An R Package to Represent Archaeological Time Series'
tags:
  - archaeology
  - time series
  - chronology
authors:
  - name: Nicolas Frerebeau
    orcid: 0000-0001-5759-4944
    affiliation: 1
  - name: Joe Roe
    orcid: 0000-0002-1011-1244
    affiliation: 2
affiliations:
 - name: Archéosciences Bordeaux (UMR 6034 - CNRS, Université Bordeaux Montaigne)
   index: 1
 - name: Institute for Archaeological Sciences, University of Bern
   index: 2
date: 7 June 2023
bibliography: paper.bib
---

# Summary

Numerous R packages have been developped to describe, analyze, and model 
temporal data in the context of archaeological studies in the broadest sense. 
These packages encompass various functionalities, including handling radiocarbon 
data (e.g., 'Bchron' by @R-Bchron or 'rcarbon' by @R-rcarbon), Optically 
Stimulated Luminescence dating ('Luminescence' by @R-Luminescence), Bayesian 
chronological modeling ('ArchaeoPhases' by @R-ArchaeoPhases), 
using paleoenvironmental proxies (e.g., 'shoredate' by @R-shoredate), or other 
temporal data (e.g., 'kairos' by @R-kairos). This multitude of packages 
underscores the significance of computational approaches in archaeology 
[@schmidt2020]. However, it also presents a major challenge as each package 
employs its own representation of temporal information. Consequently, exchanging 
data between different packages within the same data workflow becomes even more 
arduous. 'aion' is designed to provide a consistent framework for representing 
archaeological time series.

# Statement of need

R ships with a lot of functionality useful for time series, in particular 
in the base `stats` [@R-stats], or in the `zoo` [@R-zoo] packages. However, 
these features are not adapted to most archaeological time series. 
Archaeological data is typically collected through field excavations or 
surveys, resulting in irregularly spaced observation times. Although several 
packages can handle irregular time series, the way they represent dates means 
they cannot easily be used for archaeological series. These are indeed defined 
for a given calendar era and more importantly they can involve dates very far 
in the past.

`aion` provides a system of classes and methods to represent and work with such 
time series. This package does not provide tools for temporal analysis or 
modeling. Instead, it offers a system of classes and methods to represent and 
work with archaeological time series. This API can be extended and used by other 
specialized packages.

# Functionality

In base R, dates are represented by default as the number of days since 
1970-01-01 (Gregorian), with negative values for earlier dates. `aion` uses a 
different approach: it allows to create date vectors represented as *rata die* 
[@reingold2018], i.e. as number of days since 01-01-01 (Gregorian).
This allows to represent dates independently of any calendar and makes 
calculations and comparisons easier.

In `aion` a time series is represented by an S4 class that inherits from the 
base array. A time series object is an $n \times m \times p$ array, with $n$ 
being the number of observations, $m$ being the number of series and with the 
$p$ columns of the third dimension containing extra variables for each series. 
This array comes with an extra `time` slot that store the observations times 
expressed in *rata die*.

All output produced by `aion` can be formatted with (virtually) any calendar, 
as long as the calendar has been defined and the associated conversion methods 
are available. `aion` natively supports both Julian and Gregorian calendars 
(with the most common eras for the latter, e.g. Before 2000, Before Present, 
(Before) Common Era...) and allows to create custom calendars.

# References
