# aion 1.3.0.9000

# aion 1.3.0
## New classes and methods
* Add `calendar_year()` to get the average length of the year of a calendar (in solar days).

## Enhancements
* Improve French translation.

# aion 1.2.0
## Enhancements
* Translate into French.

## Bugfixes & changes
* Remove year zero ticks in plots (Julian calendar).

## Internals
* Epochs of the Gregorian era are no longer represented as subclasses, but as instances of the `GregorianCalendar` class.

# aion 1.1.0
## New classes and methods
* Add support for time intervals.

## Bugfixes & changes
* Fix a bug in `span()`: if a calendar was specified, the result was shifted according to the calendar's epoch (dates were returned instead of durations).

# aion 1.0.4
* Post publication CRAN release.

# aion 1.0.3
* Published in the [*Journal of Open Source Software*](https://doi.org/10.21105/joss.06210).

# aion 1.0.2
## Bugfixes & changes
* Rename the `format` argument of `format()` to `prefix`.

# aion 1.0.1
## Bugfixes & changes
* `plot()` now supports missing values.

# aion 1.0.0

* First release.
