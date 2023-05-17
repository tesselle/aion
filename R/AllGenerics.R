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
#' @param i,j Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
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
#' @param x description
#' @param format A [`character`] string specifying the format.
#'  It must be one of "`a`", "`ka`", "`Ma`" or "`Ga`".
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
#' @param object A [`numeric`] `vector` or `matrix` of the observed time-series
#'  values. A [`data.frame`] will be coerced to a `numeric` `matrix` via
#'  [data.matrix()].
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
#' * `time()` creates the vector of times at which a time series was sampled
#'    (expressed in *rata die*).
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

#' Plot Time Series
#'
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param panel A [`function`] in the form `function(x, y, ...)` which gives the
#'  action to be carried out in each panel of the display. The default is
#'  [graphics::lines()].
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
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
#'  `plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-plot.R
#' @seealso [graphics::plot()]
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @name plot
#' @rdname plot
NULL

# Heat Map
#
# @param x A [`TimeSeries-class`] object.
# @param calendar A [`TimeScale-class`] object specifying the target calendar
#  (see [calendar()]).
# @param col	A vector of colors such as that generated by
#  [grDevices::hcl.colors()], [grDevices::gray.colors()] or similar functions.
# @param ... Further [graphical parameters][graphics::par] to be passed to
#  [graphics::plot()].
# @return
#  `image()` is called it for its side-effects: it results in a graphic
#  being displayed. Invisibly returns `x`.
# @example inst/examples/ex-plot.R
# @seealso [graphics::image()]
# @author N. Frerebeau
# @docType methods
# @family time series tools
# @name image
# @rdname image
# NULL

# Interval Analysis ============================================================
## Allen Interval Algebra ------------------------------------------------------
#' Allen Relation Between Definite Intervals
#'
#' @param x,y A [`numeric`] vector giving the lower and upper boundaries of the
#'  time intervals, respectively. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @details
#'
#' \tabular{lrlr}{
#'  **Relation** \tab     \tab     \tab  **Converse** \cr
#'  precedes     \tab (p) \tab (P) \tab   preceded by \cr
#'  meets        \tab (m) \tab (M) \tab        met by \cr
#'  overlaps     \tab (o) \tab (O) \tab overlapped by \cr
#'  finished by  \tab (F) \tab (f) \tab      finishes \cr
#'  contains     \tab (D) \tab (d) \tab        during \cr
#'  starts       \tab (s) \tab (S) \tab    started by \cr
#'  equals       \tab (e) \tab     \tab               \cr
#' }
#'
#' @return
#'  A [`character`] matrix specifying the Allen relations.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#'
#'  Alspaugh, T. (2019). Allen's Interval Algebra.
#'  URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_relation-method
setGeneric(
  name = "allen_relation",
  def = function(x, y, ...) standardGeneric("allen_relation")
)

#' Complement of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_complement-method
setGeneric(
  name = "allen_complement",
  def = function(x, ...) standardGeneric("allen_complement")
)

#' Converse of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_converse-method
setGeneric(
  name = "allen_converse",
  def = function(x, ...) standardGeneric("allen_converse")
)

#' Composition of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_composition-method
setGeneric(
  name = "allen_composition",
  def = function(x, y, ...) standardGeneric("allen_composition")
)

#' Intersection of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_intersect-method
setGeneric(
  name = "allen_intersect",
  def = function(x, y, ...) standardGeneric("allen_intersect")
)

#' Union of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_union-method
setGeneric(
  name = "allen_union",
  def = function(x, y, ...) standardGeneric("allen_union")
)
