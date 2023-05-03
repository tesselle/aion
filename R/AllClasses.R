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
#' @docType class
#' @aliases GregorianCalendar-class
#' @keywords internal
.GregorianCalendar <- setClass(
  Class = "GregorianCalendar",
  prototype = list(
    year = 365.2425
  ),
  contains = "TimeScale"
)

#' @rdname GregorianCalendar-class
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

#' @rdname GregorianCalendar-class
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

#' @rdname GregorianCalendar-class
.BC <- setClass(
  Class = "BC",
  prototype = list(
    label = "BC",
    name = "Before Christ",
    epoch = 1,
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' @rdname GregorianCalendar-class
.BCE <- setClass(
  Class = "BCE",
  prototype = list(
    label = "BCE",
    name = "Before Common Era",
    epoch = 1,
    direction = -1L
  ),
  contains = "GregorianCalendar"
)

#' @rdname GregorianCalendar-class
.AD <- setClass(
  Class = "AD",
  prototype = list(
    label = "AD",
    name = "Anno Domini",
    epoch = 0,
    direction = 1L
  ),
  contains = "GregorianCalendar"
)

#' @rdname GregorianCalendar-class
.CE <- setClass(
  Class = "CE",
  prototype = list(
    label = "CE",
    name = "Common Era",
    epoch = 0,
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
    year = 365.25
  ),
  contains = "TimeScale"
)

# Time Series ==================================================================
#' TimeLine
#'
#' An S4 class to represent a vector of years.
#' @slot .Data A [`numeric`] vector giving the year values.
#' @slot error A [`numeric`] vector of uncertainties.
#' @slot scale A [`numeric`] value specifying the number of years represented by
#'  one unit. It should be a power of 10 (i.e. 1000 means ka).
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
    error = "numeric",
    scale = "numeric",
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
