# CLASSES DEFINITION AND INITIALIZATION
NULL

# Time Scales ==================================================================
#' Calendar
#'
#' An S4 class to represent a year unit.
#' @slot unit A [`character`] string giving the name of the year unit.
#' @slot days A [`numeric`] vector giving the average length of the year in
#'  solar days.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases Calendar-class
#' @keywords internal
.Calendar <- setClass(
  Class = "Calendar",
  slots = c(
    unit = "character",
    days = "numeric"
  )
)

#' TimeScale
#'
#' An S4 class to represent a time scale.
#' @slot label A [`character`] string giving the abbreviated label of the time
#'  scale.
#' @slot name A [`character`] string giving the name of the time scale.
#' @slot epoch A [`numeric`] value giving the epoch year from which years are
#'  counted (in Gregorian astronomical years).
#' @slot scale An [`integer`] specifying the number of years represented by one
#'  unit.
#' @slot direction An [`integer`] specifying if years are counted backwards
#'  (\eqn{-1}) or forwards (\eqn{1}) from `epoch`.
#' @note This class inherits from [`Calendar-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases TimeScale-class
#' @keywords internal
.TimeScale <- setClass(
  Class = "TimeScale",
  slots = c(
    label = "character",
    name = "character",
    epoch = "numeric",
    scale = "integer",
    direction = "integer"
  ),
  contains = "Calendar"
)

# 14C calibration ==============================================================
#' Calibrated Radiocarbon Ages
#'
#' An S4 class to represent calibrated radiocarbon ages.
#' @param labels A [`character`] vector specifying the names of the ages (e.g.
#'  laboratory codes).
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param curves A [`character`] vector specifying the calibration curves
#'  used.
#' @slot start A length-one [`numeric`] vector specifying the beginning of the
#'  calibrated time range.
#' @slot resolution A length-one [`numeric`] vector specifying the resolution of
#'  the calibrated time range.
#' @slot F14C A [`logical`] scalar:
#' @slot calendar A [`TimeScale-class`] object specfiying the time scale.
#' @slot status An [`integer`] vector specifying the calibration status.
#'  It must be one of "`0`" (OK), "`1`" (out of calibration range) or "`2`"
#'  (may extend out of calibration range).
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedAges-class
#' @keywords internal
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    labels = "character",
    ages = "numeric",
    errors = "numeric",
    curves = "character",
    start = "numeric",
    resolution = "numeric",
    F14C = "logical",
    calendar = "TimeScale",
    status = "integer"
  ),
  contains = "matrix"
)

#' Calibrated SPD
#'
#' An S4 class to represent summed probability distributions (SPD) of
#' radiocarbon dates.
#' @slot start A length-one [`numeric`] vector specifying the beginning of the
#'  calibrated time range.
#' @slot resolution A length-one [`numeric`] vector specifying the resolution of
#'  the calibrated time range.
#' @slot calendar A [`TimeScale-class`] object specfiying the time scale.
#' @note
#'  This class inherits from [`numeric`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CalibratedSPD-class
#' @keywords internal
.CalibratedSPD <- setClass(
  Class = "CalibratedSPD",
  slots = c(
    start = "numeric",
    resolution = "numeric",
    calendar = "TimeScale"
  ),
  contains = "numeric"
)
