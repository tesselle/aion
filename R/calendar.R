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
#' @aliases calendar,TimeLine-method
setMethod(
  f = "calendar",
  signature = "TimeLine",
  definition = function(object) object@calendar
)

#' @export
#' @rdname calendar
#' @aliases calendar,TimeSeries-method
setMethod(
  f = "calendar",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(years(object))
)

# Mutators =====================================================================
fixed <- function(object) {
  object@fixed
}

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
#' @aliases calendar_label,TimeLine-method
setMethod(
  f = "calendar_label",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_label,TimeSeries-method
setMethod(
  f = "calendar_label",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
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
#' @aliases calendar_name,TimeLine-method
setMethod(
  f = "calendar_name",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,TimeSeries-method
setMethod(
  f = "calendar_name",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_calendar,TimeScale-method
setMethod(
  f = "calendar_calendar",
  signature = "TimeScale",
  definition = function(object) {
    if (is_gregorian(object)) return("Gregorian")
    if (is_julian(object)) return("Julian")
    return("Undefined")
  }
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_calendar,TimeLine-method
setMethod(
  f = "calendar_calendar",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_calendar,TimeSeries-method
setMethod(
  f = "calendar_calendar",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
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
#' @aliases calendar_epoch,TimeLine-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,TimeSeries-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
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
#' @aliases calendar_direction,TimeLine-method
setMethod(
  f = "calendar_direction",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeSeries-method
setMethod(
  f = "calendar_direction",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeScale-method
setMethod(
  f = "calendar_year",
  signature = "TimeScale",
  definition = function(object) object@year
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeLine-method
setMethod(
  f = "calendar_year",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(calendar(object))
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeSeries-method
setMethod(
  f = "calendar_year",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(calendar(object))
)
