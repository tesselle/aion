# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname calendar
#' @aliases calendar,character-method
setMethod(
  f = "calendar",
  signature = "character",
  definition = function(object) {
    switch (
      object,
      BP = .BP(),
      b2k = .b2k(),
      BC = .BC(),
      BCE = .BCE(),
      AD = .AD(),
      CE = .CE(),
      stop(sprintf("Unknown calendar: %s", object), call. = FALSE)
    )
  }
)

#' @export
#' @rdname calendar
BP <- function(...) calendar("BP")

#' @export
#' @rdname calendar
b2k <- function(...) calendar("b2k")

#' @export
#' @rdname calendar
BC <- function(...) calendar("BC")

#' @export
#' @rdname calendar
BCE <- function(...) calendar("BCE")

#' @export
#' @rdname calendar
AD <- function(...) calendar("AD")

#' @export
#' @rdname calendar
CE <- function(...) calendar("CE")

# Mutators =====================================================================
calendar_fixed <- function(object) object@fixed

## Getters ---------------------------------------------------------------------
#' @export
#' @rdname calendar_get
#' @aliases calendar_label,TimeScale-method
setMethod(
  f = "calendar_label",
  signature = "TimeScale",
  definition = function(object) object@label
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,TimeScale-method
setMethod(
  f = "calendar_name",
  signature = "TimeScale",
  definition = function(object) object@name
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_unit,TimeScale-method
setMethod(
  f = "calendar_unit",
  signature = "TimeScale",
  definition = function(object) {
    if (is_gregorian(object)) return("Gregorian")
    if (is_julian(object)) return("Julian")
    return("Undefined")
  }
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,TimeScale-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeScale",
  definition = function(object) object@epoch
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeScale-method
setMethod(
  f = "calendar_direction",
  signature = "TimeScale",
  definition = function(object) sign(object@direction)
)

# @export
# @rdname calendar_get
# @aliases calendar_year,TimeScale-method
# setMethod(
#   f = "calendar_year",
#   signature = "TimeScale",
#   definition = function(object) object@year
# )
