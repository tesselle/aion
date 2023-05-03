# TIME SCALE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname era
#' @aliases era,character-method
setMethod(
  f = "era",
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
      stop(sprintf("Unknown era: %s", object), call. = FALSE)
    )
  }
)

#' @export
#' @rdname era
#' @aliases era,TimeLine-method
setMethod(
  f = "era",
  signature = "TimeLine",
  definition = function(object) object@calendar
)

#' @export
#' @rdname era
#' @aliases era,TimeSeries-method
setMethod(
  f = "era",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(years(object))
)

# Mutators =====================================================================
## Getters ---------------------------------------------------------------------
#' @export
#' @rdname era_get
#' @aliases era_label,TimeScale-method
setMethod(
  f = "era_label",
  signature = "TimeScale",
  definition = function(object) object@label
)

#' @export
#' @rdname era_get
#' @aliases era_label,TimeLine-method
setMethod(
  f = "era_label",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_label,TimeSeries-method
setMethod(
  f = "era_label",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_name,TimeScale-method
setMethod(
  f = "era_name",
  signature = "TimeScale",
  definition = function(object) object@name
)

#' @export
#' @rdname era_get
#' @aliases era_name,TimeLine-method
setMethod(
  f = "era_name",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_name,TimeSeries-method
setMethod(
  f = "era_name",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_epoch,TimeScale-method
setMethod(
  f = "era_epoch",
  signature = "TimeScale",
  definition = function(object) object@epoch
)

#' @export
#' @rdname era_get
#' @aliases era_epoch,TimeLine-method
setMethod(
  f = "era_epoch",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_epoch,TimeSeries-method
setMethod(
  f = "era_epoch",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_direction,TimeScale-method
setMethod(
  f = "era_direction",
  signature = "TimeScale",
  definition = function(object) sign(object@direction)
)

#' @export
#' @rdname era_get
#' @aliases era_direction,TimeLine-method
setMethod(
  f = "era_direction",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_direction,TimeSeries-method
setMethod(
  f = "era_direction",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_year,TimeScale-method
setMethod(
  f = "era_year",
  signature = "TimeScale",
  definition = function(object) object@year
)

#' @export
#' @rdname era_get
#' @aliases era_year,TimeLine-method
setMethod(
  f = "era_year",
  signature = "TimeLine",
  definition = function(object) methods::callGeneric(era(object))
)

#' @export
#' @rdname era_get
#' @aliases era_year,TimeSeries-method
setMethod(
  f = "era_year",
  signature = "TimeSeries",
  definition = function(object) methods::callGeneric(era(object))
)
