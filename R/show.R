# SHOW

# Format =======================================================================
setMethod(
  f = "format",
  signature = "TimeLine",
  definition = function(x) {
    prefix <- c(ka = 10^3, Ma = 10^6, Ga = 10^9)
    prefix <- names(prefix)[prefix == x@scale]
    if (length(prefix) == 0) return(era_label(x))
    sprintf("%s %s", prefix, era_label(x))
  }
)

# Show =========================================================================
setMethod(
  f = "show",
  signature = "TimeScale",
  definition = function(object) {
    dirout <- if (era_direction(object) > 0) "forwards" else "backwards"
    msg <- "%s (%s): %s years (%g days) counted %s from %g."
    msg <- sprintf(msg, era_name(object), era_label(object),
                   era_calendar(object),
                   era_year(object), dirout, era_epoch(object))
    cat(msg, sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "TimeLine",
  definition = function(object) {
    methods::callGeneric(object = era(object))
    methods::callGeneric(object = methods::as(object, "numeric", strict = TRUE))
  }
)

setMethod(
  f = "show",
  signature = "TimeSeries",
  definition = function(object) {
    n <- ncol(object)
    msg <- "%d time series observed between %g and %g %s."
    msg <- sprintf(msg, n, start(object), end(object), format(object@time))
    cat(msg, sep = "\n")
  }
)
