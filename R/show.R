# SHOW

# Pretty =======================================================================
#' @export
#' @rdname pretty
setMethod(
  f = "pretty",
  signature = "RataDie",
  definition = function(x, calendar = getOption("aion.calendar"), ...) {
    if (is.null(calendar)) return(pretty(as.numeric(x), ...))

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
  definition = function(x, prefix = c("a", "ka", "Ma", "Ga"), label = TRUE,
                        calendar = getOption("aion.calendar")) {
    if (is.null(calendar)) return(format(as.numeric(x)))

    y <- as_year(x, calendar = calendar)

    ## Scale
    if (isTRUE(prefix)) {
      power <- 10^floor(log10(abs(mean(y, na.rm = TRUE))))
      if (prefix < 10^4) prefix <- "a"
      if (power >= 10^4 && power < 10^6) prefix <- "ka"
      if (power >= 10^6 && power < 10^9) prefix <- "Ma"
      if (power >= 10^9) prefix <- "Ga"
    }
    prefix <- match.arg(prefix, several.ok = FALSE)
    power <- switch (prefix, ka = 10^3, Ma = 10^6, Ga = 10^9, 1)

    prefix <- if (power > 1) sprintf(" %s", prefix) else ""
    label <- if (isTRUE(label)) sprintf(" %s", calendar_label(calendar)) else ""
    sprintf("%g%s%s", y / power, prefix, label)
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
    n <- dim(object)
    start <- format(start(object))
    end <- format(end(object))
    msg <- "%d x %d x %d time series observed between %s and %s r.d."
    msg <- sprintf(msg, n[1L], n[2L], n[3L], start, end)
    cat(msg, sep = "\n")
  }
)
