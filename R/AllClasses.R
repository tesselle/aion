# CLASSES DEFINITION AND INITIALIZATION
NULL

# Time Scales ==================================================================
#' TimeScale
#'
#' A virtual S4 class to represent a calendar.
#' @slot label A [`character`] string specifying the abbreviated label of
#'  the time scale.
#' @slot name A [`character`] string specifying the name of the time scale.
#' @slot epoch A [`numeric`] value specifying the epoch year from which
#'  years are counted (starting date of the calendar, in years). Allows to
#'  define multiple era of a calendar.
#' @slot fixed A [`numeric`] value specifying the reference date of the calendar
#'  (in *rata die*).
#' @slot direction An [`integer`] specifying if years are counted backwards
#'  (`-1`) or forwards (`1`) from `epoch`.
#' @slot year A [`numeric`] value giving the average length of the year in
#'  solar days.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @docType class
#' @aliases TimeScale-class
#' @exportClass TimeScale
.TimeScale <- setClass(
  Class = "TimeScale",
  slots = c(
    label = "character",
    name = "character",
    epoch = "numeric",
    fixed = "numeric",
    direction = "integer",
    year = "numeric"
  ),
  contains = "VIRTUAL"
)

## Gregorian TimeScale ---------------------------------------------------------
#' GregorianCalendar
#'
#' An S4 class to represent a Gregorian calendar.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @docType class
#' @aliases GregorianCalendar-class
#' @exportClass GregorianCalendar
.GregorianCalendar <- setClass(
  Class = "GregorianCalendar",
  prototype = list(
    epoch = 0,
    direction = 1L,
    fixed = 1, # January 1, 1 at midnight
    year = 365.2425
  ),
  contains = "TimeScale"
)

## Julian TimeScale ------------------------------------------------------------
#' JulianCalendar
#'
#' An S4 class to represent a Julian calendar.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @docType class
#' @aliases JulianCalendar-class
#' @exportClass JulianCalendar
.JulianCalendar <- setClass(
  Class = "JulianCalendar",
  prototype = list(
    epoch = 1,
    direction = 1L,
    fixed = -1,
    year = 365.25
  ),
  contains = "TimeScale"
)

# Fixed Dates ==================================================================
#' RataDie
#'
#' An S4 class to represent a vector of *rata die*.
#' @slot .Data A [`numeric`] vector giving the *rata die* values.
#' @details
#'  *Rata die* (fixed date) are represented as the number of days since 01-01-01
#'  (Gregorian), with negative values for earlier dates.
#'
#'  It is intended that the date should be an integer value, but this is not
#'  enforced in the internal representation.
#' @note
#'  This class inherits from [`numeric`].
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases RataDie-class
#' @exportClass RataDie
.RataDie <- setClass(
  Class = "RataDie",
  contains = "numeric"
)

# Time Series ==================================================================
#' TimeSeries
#'
#' An S4 class to represent time series.
#' @slot .Data A \eqn{n x m x p}{n \times m \times p} `numeric` [`array`]
#'  giving the observed time-series values.
#' @slot .Time A length-\eqn{n} [`RataDie-class`] object.
#' @details
#'  A time series object is an \eqn{n x m x p}{n \times m \times p} array, with
#'  \eqn{n} being the number of observations, \eqn{m} being the number of series
#'  and with the \eqn{p} columns of the third dimension containing extra
#'  variables for each series.
#' @note
#'  This class inherits from [`array`].
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases TimeSeries-class
#' @exportClass TimeSeries
.TimeSeries <- setClass(
  Class = "TimeSeries",
  slots = c(
    .Time = "RataDie"
  ),
  contains = "array"
)

# Time Intervals ===============================================================
#' TimeIntervals
#'
#' An S4 class to represent time intervals.
#' @slot .Id A [`character`] vector specifying the identifier/name of intervals.
#'  Duplicated values are interpreted as disjoint intervals referring to the
#'  same event.
#' @slot .Start A [`RataDie-class`] object giving the start time of the
#'  intervals.
#' @slot .End A [`RataDie-class`] object giving the end time of the intervals.
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases TimeIntervals-class
#' @exportClass TimeIntervals
.TimeIntervals <- setClass(
  Class = "TimeIntervals",
  slots = c(
    .Id = "character",
    .Start = "RataDie",
    .End = "RataDie"
  )
)
