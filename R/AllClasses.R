# CLASSES DEFINITION AND INITIALIZATION
NULL

# Time Scales ==================================================================
#' TimeScale
#'
#' An S4 class to represent a time scale.
#' @slot .Data A `numeric` [`matrix`] giving the the observed time-series
#'  values.
#' @slot era_label A [`character`] string specifying the abbreviated label of
#'  the time scale.
#' @slot era_name A [`character`] string specifying the name of the time scale.
#' @slot era_epoch A [`numeric`] value specifying the epoch year from which
#'  years are counted (in Gregorian astronomical years).
#' @slot era_scale An [`integer`] specifying the number of years represented by
#'  one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @slot era_direction An [`integer`] specifying if years are counted backwards
#'  (`-1`) or forwards (`1`) from `epoch`.
#' @slot era_unit A [`character`] string specifying the name of the year unit.
#' @slot era_days A [`numeric`] vector giving the average length of the year in
#'  solar days.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases TimeScale-class
#' @keywords internal
.TimeScale <- setClass(
  Class = "TimeScale",
  slots = c(
    era_label = "character",
    era_name = "character",
    era_epoch = "numeric",
    era_scale = "integer",
    era_direction = "integer",
    era_unit = "character",
    era_days = "numeric"
  )
)

# Time Series ==================================================================
#' TimeSeries
#'
#' An S4 class to represent time series.
#' @slot .Data A `numeric` [`matrix`] giving the the observed time-series
#'  values.
#' @slot era_label A [`character`] string specifying the abbreviated label of
#'  the time scale.
#' @slot era_name A [`character`] string specifying the name of the time scale.
#' @slot era_epoch A [`numeric`] value specifying the epoch year from which
#'  years are counted (in Gregorian astronomical years).
#' @slot era_scale An [`integer`] specifying the number of years represented by
#'  one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @slot era_direction An [`integer`] specifying if years are counted backwards
#'  (`-1`) or forwards (`1`) from `epoch`.
#' @slot era_unit A [`character`] string specifying the name of the year unit.
#' @slot era_days A [`numeric`] vector giving the average length of the year in
#'  solar days.
#' @slot time_labels A [`character`] string specifying the names of the time
#'  series.
#' @slot time_start A [`numeric`] value specifying the year of the
#'  first observation.
#' @slot time_frequency A [`numeric`] value specifying the time difference
#'  between two observations (resolution), in years.
#' @details
#'  It is a matrix that represents data sampled at equidistant points in time,
#'  according to a given time scale.
#' @note
#'  This class inherits from [`matrix`] and [`TimeScale-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases TimeSeries-class
#' @keywords internal
#' @exportClass TimeSeries
.TimeSeries <- setClass(
  Class = "TimeSeries",
  slots = c(
    time_labels = "character",
    time_start = "numeric",
    time_frequency = "numeric"
  ),
  contains = c("matrix", "TimeScale")
)
