# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname as_calendar
as_gregorian <- function(label, name, epoch, direction) {
  .GregorianCalendar(
    label = label,
    name = name,
    epoch = epoch,
    direction = as.integer(sign(direction))
  )
}

#' @export
#' @rdname as_calendar
as_julian <- function(...) {
  .JulianCalendar()
}

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
  definition = function(object) {
    object@calendar
  }
)

#' @export
#' @rdname calendar
#' @aliases calendar,TimeSeries-method
setMethod(
  f = "calendar",
  signature = "TimeSeries",
  definition = function(object) {
    methods::callGeneric(object = object@time)
  }
)

# Mutators =====================================================================
## Getters ---------------------------------------------------------------------
#' @export
#' @rdname calendar_get
#' @aliases calendar_label,Calendar-method
setMethod(
  f = "calendar_label",
  signature = "Calendar",
  definition = function(object) object@label
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_label,TimeLine-method
setMethod(
  f = "calendar_label",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(object@calendar)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_label,TimeSeries-method
setMethod(
  f = "calendar_label",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(object@time)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,Calendar-method
setMethod(
  f = "calendar_name",
  signature = "Calendar",
  definition = function(object) object@name
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,TimeLine-method
setMethod(
  f = "calendar_name",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(object@calendar)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_name,TimeSeries-method
setMethod(
  f = "calendar_name",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(object@time)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,Calendar-method
setMethod(
  f = "calendar_epoch",
  signature = "Calendar",
  definition = function(object) object@epoch
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,TimeLine-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(object@calendar)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_epoch,TimeSeries-method
setMethod(
  f = "calendar_epoch",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(object@time)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,Calendar-method
setMethod(
  f = "calendar_direction",
  signature = "Calendar",
  definition = function(object) sign(object@direction)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeLine-method
setMethod(
  f = "calendar_direction",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(object@calendar)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_direction,TimeSeries-method
setMethod(
  f = "calendar_direction",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(object@time)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,Calendar-method
setMethod(
  f = "calendar_year",
  signature = "Calendar",
  definition = function(object) object@year
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeLine-method
setMethod(
  f = "calendar_year",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(object@calendar)
)

#' @export
#' @rdname calendar_get
#' @aliases calendar_year,TimeSeries-method
setMethod(
  f = "calendar_year",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(object@time)
)
