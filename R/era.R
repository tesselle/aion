# TIME SCALE
#' @include AllGenerics.R
NULL

as_era <- function(label = character(), name = character(),
                   epoch = numeric(), scale = integer(), direction = integer(),
                   unit = character(), days = numeric()) {
  .TimeScale(
    era_label = label,
    era_name = name,
    era_epoch = epoch,
    era_scale = scale,
    era_direction = direction,
    era_unit = unit,
    era_days = days
  )
}

#' @export
#' @rdname era
#' @aliases era,character-method
setMethod(
  f = "era",
  signature = "character",
  definition = function(object) {
    ## Validation
    x <- match.arg(object, names(.era), several.ok = FALSE)
    x <- .era[[x]]
    u <- match.arg(x$unit, names(.unit), several.ok = FALSE)
    u <- .unit[[u]]

    as_era(
      label = x$label,
      name = x$name,
      epoch = x$epoch,
      scale = x$scale,
      direction = x$direction,
      unit = u$unit,
      days = u$length
    )
  }
)

#' @export
#' @rdname era
#' @aliases era,TimeSeries-method
setMethod(
  f = "era",
  signature = "TimeSeries",
  definition = function(object) {
    methods::as(object, "TimeScale", strict = TRUE)
  }
)

# Mutators =====================================================================
## Getters ---------------------------------------------------------------------
#' @export
#' @rdname era_get
#' @aliases era_label,TimeScale-method
setMethod(
  f = "era_label",
  signature = "TimeScale",
  definition = function(object) object@era_label
)

#' @export
#' @rdname era_get
#' @aliases era_name,TimeScale-method
setMethod(
  f = "era_name",
  signature = "TimeScale",
  definition = function(object) object@era_name
)

#' @export
#' @rdname era_get
#' @aliases era_epoch,TimeScale-method
setMethod(
  f = "era_epoch",
  signature = "TimeScale",
  definition = function(object) object@era_epoch
)

#' @export
#' @rdname era_get
#' @aliases era_scale,TimeScale-method
setMethod(
  f = "era_scale",
  signature = "TimeScale",
  definition = function(object) object@era_scale
)

#' @export
#' @rdname era_get
#' @aliases era_direction,TimeScale-method
setMethod(
  f = "era_direction",
  signature = "TimeScale",
  definition = function(object) object@era_direction
)

#' @export
#' @rdname era_get
#' @aliases era_unit,TimeScale-method
setMethod(
  f = "era_unit",
  signature = "TimeScale",
  definition = function(object) object@era_unit
)

#' @export
#' @rdname era_get
#' @aliases era_days,TimeScale-method
setMethod(
  f = "era_days",
  signature = "TimeScale",
  definition = function(object) object@era_days
)

# Show =========================================================================
setMethod(
  f = "format",
  signature = "TimeScale",
  definition = function(x) {
    prefix <- c(ka = 10^3, Ma = 10^6, Ga = 10^9)
    prefix <- names(prefix)[prefix == era_scale(x)]
    if (length(prefix) == 0) return(era_label(x))
    sprintf("%s %s", prefix, era_label(x))
  }
)

setMethod(
  f = "show",
  signature = "TimeScale",
  definition = function(object) {
    dirout <- if (era_direction(object) > 0) "forwards" else "backwards"
    msg <- "%s (%s): %s years (%g days) counted %s from %g."
    msg <- sprintf(msg, era_name(object), era_label(object), era_unit(object),
            era_days(object), dirout, era_epoch(object))
    cat(msg, sep = "\n")
  }
)

# Convert ======================================================================
#' @export
#' @rdname convert
#' @aliases convert,character,character-method
setMethod(
  f = "convert",
  signature = c(from = "character", to = "character"),
  definition = function(from, to) {
    methods::callGeneric(from = era(from), to = era(to))
  }
)

#' @export
#' @rdname convert
#' @aliases convert,TimeScale,TimeScale-method
setMethod(
  f = "convert",
  signature = c(from = "TimeScale", to = "TimeScale"),
  definition = function(from, to) {
    ## Validation
    if (anyNA(era_days(from)) | anyNA(era_days(to))) {
      stop("Year length is undefined.", call. = FALSE)
    }

    fun <- function(x, precision = getOption("chronos.precision")) {
      ## Rescale to 1 (if not already)
      x <- x * era_scale(from)

      ## Transformation
      ux <- era_days(from)
      uy <- era_days(to)
      dx <- era_direction(from)
      dy <- era_direction(to)
      ex <- era_epoch(from)
      ey <- era_epoch(to)
      y <- ((ux * dx * dy * x) + (365.2425 * dy * (ex - ey))) / uy

      ## Apply destination scale
      y <- y / era_scale(to)

      ## Round to precision
      if (!is.na(precision)) {
        y <- round(y / precision) * precision
      }

      return(y)
    }

    return(fun)
  }
)
