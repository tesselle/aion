# CLASSES DEFINITION AND INITIALIZATION
NULL

# Time Scales ==================================================================
#' TimeScale
#'
#' An S4 class to represent a time scale.
#' @slot era_label A [`character`] string giving the abbreviated label of the
#'  time scale.
#' @slot era_name A [`character`] string giving the name of the time scale.
#' @slot era_epoch A [`numeric`] value giving the epoch year from which years
#'  are counted (in Gregorian astronomical years).
#' @slot era_scale An [`integer`] specifying the number of years represented by
#'  one unit. It should be a power of 10 (i.e. 1000 means ka).
#' @slot era_direction An [`integer`] specifying if years are counted backwards
#'  (`-1`) or forwards (`1`) from `epoch`.
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
#' @slot time_labels A [`character`] string specifying the names of the time
#'  series.
#' @slot time_start A [`numeric`] value specifying the year of the
#'  first observation.
#' @slot time_increment A [`numeric`] value specifying the time difference
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
.TimeSeries <- setClass(
  Class = "TimeSeries",
  slots = c(
    time_labels = "character",
    time_start = "numeric",
    time_increment = "numeric"
  ),
  contains = c("matrix", "TimeScale")
)

# 14C calibration ==============================================================
#' Calibrated Radiocarbon Ages
#'
#' An S4 class to represent calibrated radiocarbon ages.
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curves
#'  used.
#' @slot F14C A [`logical`] scalar:
#' @slot status An [`integer`] vector specifying the calibration status.
#'  It must be one of "`0`" (OK), "`1`" (out of calibration range) or "`2`"
#'  (may extend out of calibration range).
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedAges-class
#' @keywords internal
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    ages = "numeric",
    errors = "numeric",
    curves = "character",
    F14C = "logical",
    status = "integer"
  ),
  contains = "TimeSeries"
)

#' Calibrated SPD
#'
#' An S4 class to represent summed probability distributions (SPD) of
#' radiocarbon dates.
#' @note
#'  This class inherits from [`TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedSPD-class
#' @keywords internal
.CalibratedSPD <- setClass(
  Class = "CalibratedSPD",
  contains = "TimeSeries"
)
