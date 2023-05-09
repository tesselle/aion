# GENERIC METHODS
#' @include AllClasses.R
NULL

# Tools ========================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

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

## Operators -------------------------------------------------------------------
#' Arithmetic Operators
#'
#' Operators performing chronological comparison (not numerical).
#' @param e1,e2 A [`TimeLine-class`] object.
#' @details
#'  The following operators allow to perform artithmetic operations on years,
#'  **relatively to the epoch of the era** (this ensures that the results are
#'  the same, regardless of the era).
#'
#'  \tabular{ll}{
#'   **operator** \tab **meaning** \cr
#'   `+`          \tab move forward in time \cr
#'   `-`          \tab move backward in time \cr
#'  }
#'
#'  Years will be coerced to a plain numeric vector if a computation means their
#'  era no longer makes sense.
#' @return
#'  A [`logical`] vector.
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operators
#' @name arithmetic
#' @rdname arithmetic
NULL

#' Chronological Comparison
#'
#' Operators performing chronological comparison (not numerical).
#' @param e1,e2 A [`TimeLine-class`] object.
#' @details
#'  The following operators allow to compare two objects. Remember that this is
#'  a **chronological comparison** and not a numerical comparison (e.g. 5000 is
#'  numerically greater than 3000, but 5000 BP precedes 3000 BP).
#'
#'  \tabular{ll}{
#'   **operator** \tab **meaning** \cr
#'   `==`         \tab equals \cr
#'   `!=`         \tab differs \cr
#'   `>`          \tab preceded by \cr
#'   `<`          \tab precedes \cr
#'   `>=`         \tab preceded by or equals \cr
#'   `<=`         \tab precedes or equals \cr
#'  }
#' @return
#'  A [`logical`] vector.
#' @example inst/examples/ex-compare.R
#' @author N. Frerebeau
#' @docType methods
#' @family operators
#' @name compare
#' @rdname compare
NULL

# Time Scales ==================================================================
#' Era
#'
#' @param object A [`character`] string specifying the abbreviated label of
#'  the time scale (see details) or an object from which to extract the time
#'  scale.
#' @param ... Currently not used.
#' @details
#'  The following time scales are available:
#'
#'  \tabular{lll}{
#'   **label**   \tab **name**           \tab **unit**        \cr
#'   `BP`        \tab Before Present     \tab Gregorian years \cr
#'   `BC`        \tab Before Christ      \tab Gregorian years \cr
#'   `BCE`       \tab Before Common Era  \tab Gregorian years \cr
#'   `AD`        \tab Anno Domini        \tab Gregorian years \cr
#'   `CE`        \tab Common Era         \tab Gregorian years \cr
#'   `b2k`       \tab Years before 2000  \tab Gregorian years \cr
#'  }
#' @return
#'  A [`TimeScale-class`] object.
#' @note
#'  Inspired by [era::era()] by Joe Roe.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales tools
#' @aliases calendar-method
setGeneric(
  name = "calendar",
  def = function(object, ...) standardGeneric("calendar"),
  valueClass = "TimeScale"
)

#' Gregorian Calendar
#'
#' @param object A [`GregorianCalendar-class`] object.
#' @param label A [`character`] string specifying the abbreviated label of
#'  the time scale.
#' @param name A [`character`] string specifying the name of the time scale.
#' @param epoch A length-one [`numeric`] vector specifying the epoch year from
#'  which years are counted (in Gregorian astronomical years).
#' @param direction A length-one [`integer`] vector specifying if years are
#'  counted backwards (`-1`) or forwards (`1`) from `epoch`. Only the
#'  [sign][sign()] of `direction` will be retained.
#' @return
#'  * `as_gregorian()` returns a [`GregorianCalendar-class`] object.
#'  * `is_gregorian()` returns a [`logical`] scalar.
#' @note
#'  Inspired by [era::era()] by Joe Roe.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales tools
#' @name gregorian
#' @rdname gregorian
NULL

#' @rdname gregorian
#' @aliases is_gregorian-method
setGeneric(
  name = "is_gregorian",
  def = function(object) standardGeneric("is_gregorian")
)

#' Julian Calendar
#'
#' @param object A [`JulianCalendar-class`] object.
#' @return
#  * `as_julian()` returns a [`JulianCalendar-class`] object.
#'  * `is_julian()` returns a [`logical`] scalar.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales tools
#' @name julian
#' @rdname julian
NULL

#' @rdname julian
#' @aliases is_julian-method
setGeneric(
  name = "is_julian",
  def = function(object) standardGeneric("is_julian")
)

#' Era Parameters
#'
#' @param object A [`TimeScale-class`] object.
#' @example inst/examples/ex-calendar.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales tools
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
#' @aliases calendar_calendar-method
setGeneric(
  name = "calendar_calendar",
  def = function(object) standardGeneric("calendar_calendar")
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

#' @rdname calendar_get
#' @aliases calendar_year-method
setGeneric(
  name = "calendar_year",
  def = function(object) standardGeneric("calendar_year")
)

#' Calendar Converter
#'
#' Interconverts dates in a variety of calendars.
#' @param from A [`TimeScale-class`] object describing the source calendar.
#' @param to A [`TimeScale-class`] object describing the target calendar.
#' @param precision A length-one [`integer`] vector indicating the number of
#'  decimal places. Defaults to `NA`: no rounding is performed.
#' @param ... Currently not used.
#' @return
#'  A [`function`] that when called with a single numeric argument (years)
#'  converts years from one calendar era to another.
#' @note
#'  Adapted from [era::yr_transform()] by Joe Roe.
# @example inst/examples/ex-convert.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales tools
#' @aliases convert-method
#' @keywords internal
setGeneric(
  name = "convert",
  def = function(from, to, ...) standardGeneric("convert")
)

# Time Lines ===================================================================
#' Rata Die
#'
#' @param year A [`numeric`] vector of (decimal) years.
#' @param month A [`numeric`] vector of (decimal) years.
#' @param day A [`numeric`] vector of (decimal) years.
#' @param calendar A [`TimeScale-class`] object (see [calendar()]).
#' @return
#'  A [`RataDie-class`] object.
#' @example inst/examples/ex-years.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases as_fixed-method
setGeneric(
  name = "as_fixed",
  def = function(year, month, day, calendar) standardGeneric("as_fixed"),
  valueClass = "RataDie"
)

#' Year Conversion from Rata Die
#'
#' @param object A [`RataDie-class`] object (see [as_fixed()]).
#' @param calendar A [`TimeScale-class`] object (see [calendar()]).
#' @example inst/examples/ex-years.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases as_year-method
setGeneric(
  name = "as_year",
  def = function(object, calendar) standardGeneric("as_year")
)

#' Date Conversion from Rata Die
#'
#' @param object A [`RataDie-class`] object (see [as_fixed()]).
#' @param calendar A [`TimeScale-class`] object (see [calendar()]).
#' @example inst/examples/ex-years.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases as_date-method
setGeneric(
  name = "as_date",
  def = function(object, calendar) standardGeneric("as_date"),
  valueClass = "data.frame"
)

#' Year Vectors
#'
#' @param object A [`numeric`] vector of (decimal) years.
#' @param calendar A [`TimeScale-class`] object (see [calendar()]).
#' @param scale A length-one [`integer`] vector specifying the number of years
#'  represented by one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @param sort A [`logical`] scalar: should data be sorted in chronological
#'  order?
#' @param ... Currently not used.
#' @details
#'  `years()` only supports data sampled at equidistant points in time,
#'  expressed in decimal years (1950 means 1950.0, i.e. the beginning of the
#'  year 1950).
#' @return
#'  A [`TimeLine-class`] object.
#' @example inst/examples/ex-years.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases years-method
setGeneric(
  name = "years",
  def = function(object, calendar, ...) standardGeneric("years"),
  valueClass = "TimeLine"
)

# Time Series ==================================================================
#' Create Time Series
#'
#' @param object A [`numeric`] `vector` or `matrix` of the observed time-series
#'  values. A [`data.frame`] will be coerced to a `numeric` `matrix` via
#'  [data.matrix()].
#' @param time A [`numeric`] vector of (decimal) years or a [`TimeLine-class`]
#'  object (see [years()]).
#' @param calendar A [`TimeScale-class`] object (see [calendar()]). If missing,
#'  `years` must be a [`TimeLine-class`] object.
#' @param scale A length-one [`numeric`] vector specifying the number of years
#'  represented by one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @param names A [`character`] string specifying the names of the time
#'  series.
#' @param ... Currently not used.
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
#' * `time()` creates the vector of times at which a time series was sampled.
#' * `frequency()` returns the number of samples per unit time.
#' @param x A [`TimeSeries-class`] object.
#' @return
#'  A [`numeric`] vector.
#' @example inst/examples/ex-series.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases time-method
#' @name time
#' @rdname time
NULL

#' Time Series Windows
#'
#' Extracts the subset of the object `x` observed between the times `start` and
#' `end`.
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
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @name plot
#' @rdname plot
NULL

#' Change the Time Scale
#'
#' Change the time scale of time series.
#' @param object A [`TimeSeries-class`] object.
#' @param target A [`TimeSeries-class`] object serving as a template for the
#'  target calendar. Alternatively, a [`character`] string specifying the target
#'  calendar or a [`TimeScale-class`] object (see [calendar()]).
#' @param ... Currently not used.
#' @return
#'  A [`TimeSeries-class`] object.
#' @example inst/examples/ex-project.R
#' @author N. Frerebeau
#' @docType methods
#' @family time series tools
#' @aliases project-method
setGeneric(
  name = "project",
  def = function(object, target, ...) standardGeneric("project")
)

# Interval Analysis ============================================================
## HPDI ------------------------------------------------------------------------
#' HPD Regions
#'
#' @param x A [`numeric`] vector giving the coordinates of the points where
#'  the density is estimated.
#' @param y A [`numeric`] vector giving the estimated density values.
#'  If `y` is missing and `x` is a `ǹumeric` vector, density estimates will be
#'  computed from `x`.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @return
#'  A three-columns `numeric` [`matrix`] giving the lower and upper boundaries
#'  of the HPD interval and associated probabilities.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
# @example inst/examples/ex-hpdi.R
#' @author N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases interval_hdr-method
setGeneric(
  name = "interval_hdr",
  def = function(x, y, ...) standardGeneric("interval_hdr")
)

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
