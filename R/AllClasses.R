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
