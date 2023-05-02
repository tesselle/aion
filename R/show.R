# SHOW

# Format =======================================================================
setMethod(
  f = "format",
  signature = "TimeLine",
  definition = function(x) {
    prefix <- c(ka = 10^3, Ma = 10^6, Ga = 10^9)
    prefix <- names(prefix)[prefix == x@scale]
    if (length(prefix) == 0) return(calendar_label(x))
    sprintf("%s %s", prefix, calendar_label(x))
  }
)

# Show =========================================================================
setMethod(
  f = "show",
  signature = "Calendar",
  definition = function(object) {
    dirout <- if (calendar_direction(object) > 0) "forwards" else "backwards"
    msg <- "%s (%s): years (%g days) counted %s from %g."
    msg <- sprintf(msg, calendar_name(object), calendar_label(object),
                   calendar_year(object), dirout, calendar_epoch(object))
    cat(msg, sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "TimeLine",
  definition = function(object) {
    methods::callGeneric(object = calendar(object))
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
