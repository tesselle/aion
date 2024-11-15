# SHOW

# Show =========================================================================
setMethod(
  f = "show",
  signature = "TimeScale",
  definition = function(object) {
    cal_name <- calendar_name(object)
    cal_label <- calendar_label(object)
    has_name <- length(cal_name) == 1 && cal_name != ""
    has_label <- length(cal_label) == 1 && cal_label != ""

    era <- ""
    if (has_name && has_label) {
      era <- sprintf("%s (%s): ", calendar_name(object), calendar_label(object))
    }

    if (calendar_direction(object) > 0) {
      msg <- tr_("%s%s counted forwards from %g")
    } else {
      msg <- tr_("%s%s counted backwards from %g")
    }
    msg <- sprintf(msg, era, calendar_unit(object), calendar_epoch(object))
    cat(trimws(msg), sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "RataDie",
  definition = function(object) {
    msg <- tr_("Rata die: number of days since 01-01-01 (Gregorian)")
    cat(msg, sep = "\n")
    methods::callGeneric(object@.Data)
  }
)

setMethod(
  f = "show",
  signature = "TimeSeries",
  definition = function(object) {
    n <- dim(object)
    k <- n[[2]] + n[[3]] - 1
    start <- format(as_fixed(start(object)))
    end <- format(as_fixed(end(object)))
    msg <- ngettext(k, "%d x %d x %d time series observed between %s and %s",
                    "%d x %d x %d time series observed between %s and %s")
    msg <- sprintf(msg, n[[1L]], n[[2L]], n[[3L]], start, end)
    cat(msg, sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "TimeIntervals",
  definition = function(object) {
    n <- length(object)
    start <- format(as_fixed(min(start(object))))
    end <- format(as_fixed(max(end(object))))
    msg <- ngettext(n, "%d time interval observed between %s and %s",
                    "%d time intervals observed between %s and %s")
    msg <- sprintf(msg, n, start, end)
    cat(msg, sep = "\n")
  }
)
