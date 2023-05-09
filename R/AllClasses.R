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
#'  years are counted (in Gregorian astronomical years).
#' @slot direction An [`integer`] specifying if years are counted backwards
#'  (`-1`) or forwards (`1`) from `epoch`.
#' @slot year A [`numeric`] value giving the average length of the year in
#'  solar days.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @docType class
#' @aliases TimeScale-class
#' @keywords internal
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

## Gregorian TimeScale ----------------------------------------------------------
#' GregorianCalendar
#'
#' An S4 class to represent a Gregorian calendar.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @family gregorian era classes
#' @docType class
#' @aliases GregorianCalendar-class
#' @keywords internal
.GregorianCalendar <- setClass(
  Class = "GregorianCalendar",
  prototype = list(
    epoch = 0,
    fixed = 1, # January 1, 1 at midnight
    year = 365.2425
  ),
  contains = "TimeScale"
)

#' BP
#'
#' An S4 class to represent the (Gregorian) BP era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases BP-class
#' @keywords internal
.BP <- setClass(
  Class = "BP",
  prototype = list(
    label = "BP",
    name = "Before Present",
    epoch = 1950,
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' b2k
#'
#' An S4 class to represent the (Gregorian) b2k era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases b2k-class
#' @keywords internal
.b2k <- setClass(
  Class = "b2k",
  prototype = list(
    label = "b2k",
    name = "Before 2000",
    epoch = 2000,
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' BC
#'
#' An S4 class to represent the (Gregorian) BC era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases BC-class
#' @keywords internal
.BC <- setClass(
  Class = "BC",
  prototype = list(
    label = "BC",
    name = "Before Christ",
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' BCE
#'
#' An S4 class to represent the (Gregorian) BCE era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases BCE-class
#' @keywords internal
.BCE <- setClass(
  Class = "BCE",
  prototype = list(
    label = "BCE",
    name = "Before Common Era",
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' AD
#'
#' An S4 class to represent the (Gregorian) AD era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases AD-class
#' @keywords internal
.AD <- setClass(
  Class = "AD",
  prototype = list(
    label = "AD",
    name = "Anno Domini",
    direction = 1L
  ),
  contains = "GregorianCalendar"
)

#' CE
#'
#' An S4 class to represent the (Gregorian) CE era.
#' @author N. Frerebeau
#' @family classes
#' @family gregorian era classes
#' @docType class
#' @aliases CE-class
#' @keywords internal
.CE <- setClass(
  Class = "CE",
  prototype = list(
    label = "CE",
    name = "Common Era",
    direction = 1L
  ),
  contains = "GregorianCalendar"
)

## Julian TimeScale -------------------------------------------------------------
#' JulianCalendar
#'
#' An S4 class to represent a Julian calendar.
#' @author N. Frerebeau
#' @family classes
#' @family calendar classes
#' @docType class
#' @aliases JulianCalendar-class
#' @keywords internal
.JulianCalendar <- setClass(
  Class = "JulianCalendar",
  prototype = list(
    epoch = 0,
    fixed = -1721424.5,
    year = 365.25
  ),
  contains = "TimeScale"
)

# Time Series ==================================================================
#' RataDie
#'
#' An S4 class to represent a vector of *rata die*.
#' @slot .Data A [`numeric`] vector giving the *rata die* values.
#' @details
#'  *Rata die* are represented as the number of days since 01-01-01 (Gregorian),
#'  with negative values for earlier dates. They are always printed following
#'  the rules of the current Gregorian calendar.
#'
#'  It is intended that the date should be an integer value, but this is not
#'  enforced in the internal representation.
#'
#'  When printing there is assumed to be a year zero.
#' @note
#'  This class inherits from [`numeric`].
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases RataDie-class
#' @keywords internal
#' @exportClass RataDie
.RataDie <- setClass(
  Class = "RataDie",
  contains = "numeric"
)

#' TimeLine
#'
#' An S4 class to represent a vector of years.
#' @slot .Data A [`numeric`] vector giving the year values.
#' @slot calendar A [`TimeScale-class`] object specifying the time scale.
#' @note
#'  This class inherits from [`numeric`].
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases TimeLine-class
#' @keywords internal
#' @exportClass TimeLine
.TimeLine <- setClass(
  Class = "TimeLine",
  slots = c(
    calendar = "TimeScale"
  ),
  contains = "numeric"
)

#' TimeSeries
#'
#' An S4 class to represent time series.
#' @slot .Data A `numeric` [`matrix`] giving the observed time-series values.
#' @slot time A [`TimeLine-class`] object.
#' @details
#'  It is a matrix that represents data sampled at equidistant points in time,
#'  according to a given time scale.
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @family time classes
#' @docType class
#' @aliases TimeSeries-class
#' @keywords internal
#' @exportClass TimeSeries
.TimeSeries <- setClass(
  Class = "TimeSeries",
  slots = c(
    time = "TimeLine"
  ),
  contains = "matrix"
)
