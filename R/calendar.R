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
#' @rdname gregorian
BP <- function(...) {
  .GregorianCalendar(
    label = tr_("BP"),
    name = tr_("Before Present"),
    epoch = 1950,
    direction = -1L
  )
}

#' @export
#' @rdname gregorian
b2k <- function(...) {
  .GregorianCalendar(
    label = tr_("b2k"),
    name = tr_("Before 2000"),
    epoch = 2000,
    direction = -1L
  )
}

#' @export
#' @rdname gregorian
BC <- function(...) {
  .GregorianCalendar(
    label = tr_("BC"),
    name = tr_("Before Christ"),
    direction = -1L
  )
}

#' @export
#' @rdname gregorian
BCE <- function(...) {
  .GregorianCalendar(
    label = tr_("BCE"),
    name = tr_("Before Common Era"),
    direction = -1L
  )
}

#' @export
#' @rdname gregorian
AD <- function(...) {
  .GregorianCalendar(
    label = tr_("AD"),
    name = tr_("Anno Domini")
  )
}

#' @export
#' @rdname gregorian
CE <- function(...) {
  .GregorianCalendar(
    label = tr_("CE"),
    name = tr_("Common Era")
  )
}

#' @export
#' @rdname julian
J <- function(...) {
  .JulianCalendar(
    label = "",
    name = ""
  )
}

# Mutators =====================================================================
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
    if (is_gregorian(object)) return(tr_("Gregorian years"))
    if (is_julian(object)) return(tr_("Julian years"))
    return(tr_("Undefined"))
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

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeScale-method
setMethod(
  f = "calendar_year",
  signature = "TimeScale",
  definition = function(object) object@year
)
