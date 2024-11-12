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
      tolower(object),
      bp = BP(),
      b2k = b2k(),
      bc = BC(),
      bce = BCE(),
      ad = AD(),
      ce = CE(),
      julian = J(),
      stop(sprintf(tr_("Unknown calendar: %s"), object), call. = FALSE)
    )
  }
)

#' @export
#' @describeIn gregorian Gregorian BP era.
BP <- function(...) {
  .GregorianCalendar(
    label = "BP",
    name = "Before Present",
    epoch = 1950,
    direction = -1L
  )
}

#' @export
#' @describeIn gregorian Gregorian b2k era.
b2k <- function(...) {
  .GregorianCalendar(
    label = "b2k",
    name = "Before 2000",
    epoch = 2000,
    direction = -1L
  )
}

#' @export
#' @describeIn gregorian Gregorian BC era.
BC <- function(...) {
  .GregorianCalendar(
    label = "BC",
    name = "Before Christ",
    direction = -1L
  )
}

#' @export
#' @describeIn gregorian Gregorian BCE era.
BCE <- function(...) {
  .GregorianCalendar(
    label = "BCE",
    name = "Before Common Era",
    direction = -1L
  )
}

#' @export
#' @describeIn gregorian Gregorian AD era.
AD <- function(...) {
  .GregorianCalendar(
    label = "AD",
    name = "Anno Domini"
  )
}

#' @export
#' @describeIn gregorian Gregorian CE era.
CE <- function(...) {
  .GregorianCalendar(
    label = "CE",
    name = "Common Era"
  )
}

# @export
# @describeIn gregorian Gregorian AUC era.
# AUC <- function(...) {
#   .GregorianCalendar(
#     label = "AUC",
#     name = "Ab urbe condita",
#     epoch = 753,
#     direction = 1
#   )
# }

#' @export
#' @rdname julian
J <- function(...) {
  .JulianCalendar(
    label = "",
    name = ""
  )
}

# Mutators =====================================================================
calendar_year <- function(object) object@year

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
#' @aliases calendar_fixed,TimeScale-method
setMethod(
  f = "calendar_fixed",
  signature = "TimeScale",
  definition = function(object) object@fixed
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeScale-method
setMethod(
  f = "calendar_direction",
  signature = "TimeScale",
  definition = function(object) sign(object@direction)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,NULL-method
setMethod(
  f = "calendar_direction",
  signature = "NULL",
  definition = function(object) 1L
)
