# GENERIC METHODS
#' @include AllClasses.R
NULL

# Time Scales ==================================================================
#' Calendar Converter
#'
#' Interconverts dates in a variety of calendars.
#' @param from A [`TimeScale-class`] object describing the source era.
#' @param to A [`TimeScale-class`] object describing the target era.
#' @param ... Currently not used.
#' @return
#'  A [`function`] that when called with a single numeric argument (years)
#'  converts years from one calendar to another.
#' @example inst/examples/ex-era.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases convert-method
#' @keywords internal
setGeneric(
  name = "convert",
  def = function(from, to, ...) standardGeneric("convert")
)

#' Time Scales
#'
#' @param x A [`character`] string specifying the time scale (see details).
#' @param ... Currently not used.
#' @details
#'  The following time scales are available:
#'  \describe{
#'   \item{`BP`}{Before Present.}
#'   \item{`BC`}{Before Christ.}
#'   \item{`BCE`}{Before Common Era.}
#'   \item{`AD`}{Anno Domini.}
#'   \item{`CE`}{Common Era.}
#'   \item{`b2k`}{Years before 2000.}
#'  }
#' @return
#'  A [`TimeScale-class`] object.
#' @example inst/examples/ex-era.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases era-method
#' @keywords internal
setGeneric(
  name = "era",
  def = function(x, ...) standardGeneric("era")
)
