# SHOW

# Pretty =======================================================================
#' @export
#' @rdname pretty
setMethod(
  f = "pretty",
  signature = "RataDie",
  definition = function(x, calendar = getOption("chronos.calendar"), ...) {
    x <- as_year(x, calendar = calendar)
    fixed(year = pretty(x, ...), calendar = calendar)
  }
)

# Format =======================================================================
#' @export
#' @rdname format
setMethod(
  f = "format",
  signature = "TimeScale",
  definition = function(x) {
    label <- calendar_label(x)
    label <- if (length(label) > 0) sprintf(" %s", label) else ""

    sprintf("%s years%s", calendar_unit(x), label)
  }
)

#' @export
#' @rdname format
setMethod(
  f = "format",
  signature = "RataDie",
  definition = function(x, format = c("a", "ka", "Ma", "Ga"), label = TRUE,
                        calendar = getOption("chronos.calendar")) {
    y <- as_year(x, calendar = calendar)

    ## Scale
    if (isTRUE(format)) {
      power <- 10^floor(log10(abs(mean(y, na.rm = TRUE))))
      if (format < 10^4) format <- "a"
      if (power >= 10^4 && power < 10^6) format <- "ka"
      if (power >= 10^6 && power < 10^9) format <- "Ma"
      if (power >= 10^9) format <- "Ga"
    }
    format <- match.arg(format, several.ok = FALSE)
    power <- switch (format, ka = 10^3, Ma = 10^6, Ga = 10^9, 1)

    format <- if (power > 1) sprintf(" %s", format) else ""
    label <- if (isTRUE(label)) sprintf(" %s", calendar_label(calendar)) else ""
    sprintf("%g%s%s", y / power, format, label)
  }
)

# Show =========================================================================
setMethod(
  f = "show",
  signature = "TimeScale",
  definition = function(object) {
    dirout <- if (calendar_direction(object) > 0) "forwards" else "backwards"
    era <- sprintf("%s (%s): ", calendar_name(object), calendar_label(object))
    if (length(era) == 0) era <- ""

    msg <- "%s%s years counted %s from %g."
    msg <- sprintf(msg, era, calendar_unit(object), dirout, calendar_epoch(object))
    cat(msg, sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "RataDie",
  definition = function(object) {
    msg <- "Rata die: number of days since 01-01-01 (Gregorian)."
    cat(msg, sep = "\n")
    methods::callGeneric(object@.Data)
  }
)

setMethod(
  f = "show",
  signature = "TimeSeries",
  definition = function(object) {
    n <- ncol(object)
    start <- format(start(object))
    end <- format(end(object))
    msg <- "%d time series observed between %s and %s r.d."
    msg <- sprintf(msg, n, start, end)
    cat(msg, sep = "\n")
  }
)
