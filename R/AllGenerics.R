# GENERIC METHODS
#' @include AllClasses.R
NULL

# Tools ========================================================================
## Mutators --------------------------------------------------------------------
# Get or Set Parts of an Object
#
# Getters and setters to extract or replace parts of an object.
# @param x An object from which to get or set element(s).
# @param value A possible value for the element(s) of `x`.
# @return
#  An object of the same sort as `x` with the new values assigned.
# @example inst/examples/ex-mutator.R
# @author N. Frerebeau
# @docType methods
# @family mutators
# @name mutators
# @rdname mutators
# @aliases get set
# NULL

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j,k Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
# @param ... Currently not used.
#' @return
#'  A subsetted object.
# @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# Calendars ====================================================================
#' Calendar
#'
#' @param object A [`character`] string specifying the abbreviated label of
#'  the time scale (see details) or an object from which to extract the time
#'  scale.
#' @details
#'  The following time scales are available:
#'
#'  \tabular{lll}{
#'   **label**   \tab **era**            \tab **calendar** \cr
#'   `BP`        \tab Before Present     \tab Gregorian    \cr
#'   `BC`        \tab Before Christ      \tab Gregorian    \cr
#'   `BCE`       \tab Before Common Era  \tab Gregorian    \cr
#'   `AD`        \tab Anno Domini        \tab Gregorian    \cr
#'   `CE`        \tab Common Era         \tab Gregorian    \cr
#'   `b2k`       \tab Years before 2000  \tab Gregorian    \cr
#'   `julian`    \tab                    \tab Julian       \cr
#'  }
#' @return
#'  A [`TimeScale-class`] object.
#' @note
#'  Inspired by [era::era()] by Joe Roe.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @aliases calendar-method
setGeneric(
  name = "calendar",
  def = function(object) standardGeneric("calendar"),
  valueClass = "TimeScale"
)

#' Gregorian Calendar
#'
#' @param ... Currently not used.
#' @return
#'  A [`GregorianCalendar-class`] object.
#' @example inst/examples/ex-calendar.R
#' @seealso [calendar()]
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @name gregorian
#' @rdname gregorian
NULL

#' Julian Calendar
#'
#' @param ... Currently not used.
#' @return
#'  A [`JulianCalendar-class`] object.
#' @example inst/examples/ex-calendar.R
#' @seealso [calendar()]
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @name julian
#' @rdname julian
NULL

#' Is an Object a Calendar?
#'
#' Test inheritance relationships between an object and a calendar class.
#' @param object Any \R object.
#' @return
#'  A [`logical`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @name is
#' @rdname is
NULL

#' @rdname is
#' @aliases is_gregorian-method
setGeneric(
  name = "is_gregorian",
  def = function(object) standardGeneric("is_gregorian")
)

#' @rdname is
#' @aliases is_julian-method
setGeneric(
  name = "is_julian",
  def = function(object) standardGeneric("is_julian")
)

#' Calendar Parameters
#'
#' @param object A [`TimeScale-class`] object.
#' @return
#'  * `calendar_label()` returns a [`character`] string specifying the
#'     abbreviated label of the time scale.
#'  * `calendar_name()` returns a [`character`] string specifying the name of
#'     the time scale.
#'  * `calendar_unit()` returns a [`character`] string specifying the name of
#'     the calendar.
#'  * `calendar_epoch()` returns length-one [`numeric`] vector specifying the
#'     epoch year from which years are counted (in Gregorian years).
#'  * `calendar_direction()` returns a length-one [`integer`] vector specifying
#'     if years are counted backwards (\eqn{-1}) or forwards (\eqn{1}) from
#'     `epoch`. Only the [sign][sign()] of `calendar_direction()` is relevant.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @name calendar_get
#' @rdname calendar_get
NULL

#' @rdname calendar_get
#' @aliases calendar_label-method
setGeneric(
  name = "calendar_label",
  def = function(object) standardGeneric("calendar_label")
)

#' @rdname calendar_get
#' @aliases calendar_name-method
setGeneric(
  name = "calendar_name",
  def = function(object) standardGeneric("calendar_name")
)

#' @rdname calendar_get
#' @aliases calendar_unit-method
setGeneric(
  name = "calendar_unit",
  def = function(object) standardGeneric("calendar_unit")
)

#' @rdname calendar_get
#' @aliases calendar_epoch-method
setGeneric(
  name = "calendar_epoch",
  def = function(object) standardGeneric("calendar_epoch")
)

#' @rdname calendar_get
#' @aliases calendar_direction-method
setGeneric(
  name = "calendar_direction",
  def = function(object) standardGeneric("calendar_direction")
)

# @rdname calendar_get
# @aliases calendar_fixed-method
# setGeneric(
#   name = "calendar_fixed",
#   def = function(object) standardGeneric("calendar_fixed")
# )

# @rdname calendar_get
# @aliases calendar_year-method
# setGeneric(
#   name = "calendar_year",
#   def = function(object) standardGeneric("calendar_year")
# )

#' Calendar Converter
#'
#' Interconverts dates in a variety of calendars.
#' @param from A [`TimeScale-class`] object describing the source calendar.
#' @param to A [`TimeScale-class`] object describing the target calendar.
#' @param ... Currently not used.
#' @return
#'  A [`function`] that when called with a single numeric argument (factional
#'  years) converts years from one calendar to another.
#' @example inst/examples/ex-convert.R
#' @author N. Frerebeau
#' @docType methods
#' @family calendar tools
#' @aliases convert-method
setGeneric(
  name = "convert",
  def = function(from, to, ...) standardGeneric("convert")
)

# Fixed Dates ==================================================================
#' *Rata Die* (Fixed Date)
#'
#' @param year A [`numeric`] vector of years. If `month` and `day` are missing,
#'  decimal years are expected.
#' @param month A [`numeric`] vector of months.
#' @param day A [`numeric`] vector of days.
#' @param calendar A [`TimeScale-class`] object specifying the calendar of
#'  `year`, `month` and `day` (see [calendar()]).
#' @param scale A length-one [`integer`] vector specifying the number of years
#'  represented by one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @param ... Currently not used.
#' @details
#'  *Rata die* are represented as the number of days since 01-01-01 (Gregorian),
#'  with negative values for earlier dates.
#' @return
#'  A [`RataDie-class`] object.
#' @example inst/examples/ex-fixed.R
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @aliases fixed-method
setGeneric(
  name = "fixed",
  def = function(year, month, day, calendar, ...) standardGeneric("fixed"),
  valueClass = "RataDie"
)

#' Coerce to *Rata Die*
#'
#' @param from A [`numeric`] vector of *rata die*.
#' @return
#'  A [`RataDie-class`] object.
#' @example inst/examples/ex-fixed.R
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @aliases as_fixed-method
setGeneric(
  name = "as_fixed",
  def = function(from) standardGeneric("as_fixed"),
  valueClass = "RataDie"
)

#' Year Conversion from *Rata Die*
#'
#' @param object A [`RataDie-class`] object (see [fixed()]).
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param decimal A [`logical`] scalar: should decimal years be returned?
#'  If `FALSE`, the decimal part is dropped.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector of (decimal) years.
#' @example inst/examples/ex-fixed.R
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @aliases as_year-method
setGeneric(
  name = "as_year",
  def = function(object, calendar, ...) standardGeneric("as_year")
)

#' *Rata Die* Conversion to and from Gregorian Years
#'
#' Convenient functions for conversion from and to *rata die* for a given
#' Gregorian era.
#' @inheritParams fixed
#' @inheritParams as_year
#' @return
#'  * `fixed_from_*()` returns a [`RataDie-class`] object.
#'  * `fixed_to_*()` returns a [`numeric`] vector of Gregorian years.
#' @example inst/examples/ex-fixed.R
#' @details
#'  The astronomical notation is used for Gregorian years (there *is* a year 0).
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @name fixed_gregorian
#' @rdname fixed_gregorian
NULL

#' *Rata Die* Conversion to and from Julian Years
#'
#' Convenient functions for conversion from and to *rata die*.
#' @inheritParams fixed
#' @inheritParams as_year
#' @return
#'  * `fixed_from_julian()` returns a [`RataDie-class`] object.
#'  * `fixed_to_julian()` returns a [`numeric`] vector of Julian years.
#' @example inst/examples/ex-fixed.R
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @name fixed_julian
#' @rdname fixed_julian
NULL

#' Date Conversion from *Rata Die*
#'
#' @param object A [`RataDie-class`] object (see [fixed()]).
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @return
#'  A [`numeric`] vector of (decimal) years.
#' @example inst/examples/ex-fixed.R
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @aliases as_date-method
setGeneric(
  name = "as_date",
  def = function(object, calendar) standardGeneric("as_date"),
  valueClass = "data.frame"
)

#' Converts a Date to a Decimal of its Year
#'
#' @param year A [`numeric`] vector of years. If `month` and `day` are missing,
#'  decimal years are expected.
#' @param month A [`numeric`] vector of months.
#' @param day A [`numeric`] vector of days.
#' @param calendar A [`TimeScale-class`] object specifying the calendar of
#'  `year`, `month` and `day` (see [calendar()]).
#' @return
#'  A [`numeric`] vector of (ecimal years.
#' @example inst/examples/ex-fixed.R
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @aliases as_decimal-method
setGeneric(
  name = "as_decimal",
  def = function(year, month, day, calendar) standardGeneric("as_decimal")
)

#' Date Conversion to Character
#'
#' @param x A [`RataDie-class`] object.
#' @param format A [`character`] string specifying the prefix.
#'  It should be one of "`a`", "`ka`", "`Ma`" or "`Ga`".
#'  If `TRUE`, a good guess for an appropriate format is made.
#' @param label A [`logical`] scalar: should the label of the calendar be
#'  displayed?
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @return
#'  A [`character`] vector representing the date.
#' @example inst/examples/ex-fixed.R
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @name format
#' @rdname format
NULL

#' Pretty Breakpoints
#'
#' @param x A [`RataDie-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param ... Further parameters to be passed to [base::pretty()].
#' @details
#'  `pretty()` computes a vector of increasing numbers which are "pretty" in
#'  decimal notation of `calendar`. Pretty breakpoints are then converted to
#'  *rata die*.
#' @return
#'  A [`RataDie-class`] object.
#' @docType methods
#' @family fixed date tools
#' @name pretty
#' @rdname pretty
NULL

#' Arithmetic Operators
#'
#' Operators performing arithmetic operations.
#' @param e1,e2 A [`RataDie-class`] object or a [`numeric`] vector.
#' @details
#'  *Rata die* will be converted to a plain `numeric` vector if a computation no
#'  longer makes sense in temporal terms.
#' @return
#'  A [`logical`] vector.
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family fixed date tools
#' @name arithmetic
#' @rdname arithmetic
NULL

# Time Series ==================================================================
#' Create Time Series
#'
#' @param object A [`numeric`] `vector`, `matrix` or `array` of the observed
#'  time-series values.
#'  A [`data.frame`] will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param time A [`numeric`] vector of (decimal) years or a [`RataDie-class`]
#'  object (see [fixed()]).
#' @param calendar A [`TimeScale-class`] object specifying the calendar of
#'  `time` (see [calendar()]). If missing, `time` must be a [`RataDie-class`]
#'  object.
#' @param scale A length-one [`numeric`] vector specifying the number of years
#'  represented by one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @param names A [`character`] string specifying the names of the time
#'  series.
#' @param ... Currently not used.
#' @details
#'  Data will be sorted in chronological order.
#' @return
#'  A [`TimeSeries-class`] object.
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases series-method
setGeneric(
  name = "series",
  def = function(object, time, calendar, ...) standardGeneric("series"),
  valueClass = "TimeSeries"
)

#' Terminal Times of Time Series
#'
#' Get the times the first and last observations were taken.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]). If `NULL` (the default), *rata die* are returned.
#' @return
#'  A [`numeric`] vector.
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases start-method end-method
#' @name start
#' @rdname start
NULL

#' Sampling Times of Time Series
#'
#' Get the sampling times:
#' * `time()` creates the vector of times at which a time series was sampled.
#' * `frequency()` returns the mean number of samples per unit time.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]). If `NULL` (the default), *rata die* are returned.
#' @return
#'  A [`numeric`] vector.
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases time-method frequency-method
#' @name time
#' @rdname time
NULL

#' Duration of Time Series
#'
#' Get the duration.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]). If `NULL` (the default), *rata die* are returned.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases span-method
setGeneric(
  name = "span",
  def = function(x, ...) standardGeneric("span")
)

#' Time Series Windows
#'
#' Extracts the subset of the object `x` observed between the times `start` and
#' `end` (expressed in *rata die*).
#' @param x A [`TimeSeries-class`] object.
#' @param start A length-one [`numeric`] vector specifying the start time of the
#'  period of interest.
#' @param end A length-one [`numeric`] vector specifying the end time of the
#'  period of interest.
#' @return
#'  A [`TimeSeries-class`] object.
#' @example inst/examples/ex-window.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases window-method
#' @name window
#' @rdname window
NULL

#' Coerce to a Data Frame
#'
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]). If `NULL` (the default), *rata die* are returned.
#' @param ... Further parameters to be passed to [data.frame()].
#' @return
#'  A long [`data.frame`] with the following columns:
#'  \describe{
#'   \item{`time`}{The (decimal) years at which the time series was sampled.}
#'   \item{`series`}{The name of the time series.}
#'   \item{`variable`}{The name of the variables.}
#'   \item{`value`}{The observed value.}
#'  }
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @name data.frame
#' @rdname data.frame
NULL

#' Plot Time Series
#'
#' @param x A [`TimeSeries-class`] object.
#' @param facet A [`character`] string specifying whether the series should be
#'  plotted separately (with a common time axis) or on a single plot?
#'  It must be one of "`multiple`" or "`single`". Any unambiguous substring can
#'  be given.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param panel A [`function`] in the form `function(x, y, ...)`
#'  which gives the action to be carried out in each panel of the display.
#'  The default is [graphics::lines()].
#' @param flip A [`logical`] scalar: should the y-axis (ticks and numbering) be
#'  flipped from side 2 (left) to 4 (right) from series to series when `facet`
#'  is "`multiple`"?
#' @param ncol An [`integer`] specifying the number of columns to use when
#'  `facet` is "`multiple`". Defaults to 1 for up to 4 series, otherwise to 2.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further parameters to be passed to `panel`
#'  (e.g. [graphical parameters][graphics::par]).
#' @return
#'  `plot()` is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-plot.R
#' @seealso [graphics::plot()]
#' @author N. Frerebeau
#' @docType methods
#' @family plotting tools
#' @name plot
#' @rdname plot
NULL

#' Heat Map
#'
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param k An [`integer`] specifying the slice of `x` along the third
#'  dimension to be plotted.
#' @param ... Further parameters to be passed to [graphics::image()].
#' @return
#'  `image()` is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-plot.R
#' @seealso [graphics::image()]
#' @author N. Frerebeau
#' @docType methods
#' @family plotting tools
#' @name image
#' @rdname image
NULL

#' Time Series Plotting Functions
#'
#' @param side An [`integer`] specifying which side of the plot the axis is to
#'  be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and
#'  4=right.
#' @param at A [`numeric`] vector giving the points at which tick-marks are to
#'  be drawn. If `NULL`, tickmark locations are computed.
#' @param format A [`character`] string specifying the prefix.
#'  It should be one of "`a`", "`ka`", "`Ma`" or "`Ga`".
#'  If `TRUE`, a good guess for an appropriate format is made.
#' @param labels A [`logical`] scalar specifying whether annotations are to be
#'  made at the tickmarks, or a vector of [`character`] strings to be placed at
#'  the tickpoints.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param ... Further parameters to be passed to [graphics::axis()].
#'  (e.g. [graphical parameters][graphics::par]).
#' @return
#'  `year_axis()` is called it for its side-effects.
#' @example inst/examples/ex-axis.R
#' @author N. Frerebeau
#' @docType methods
#' @family plotting tools
#' @name year_axis
#' @rdname year_axis
NULL
