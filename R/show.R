# SHOW

# Pretty =======================================================================
#' @export
#' @method pretty RataDie
pretty.RataDie <- function(x, calendar = getOption("aion.calendar"), ...) {
  if (is.null(calendar)) return(pretty(as.numeric(x), ...))

  x <- as_year(x, calendar = calendar)
  fixed(year = pretty(x, ...), calendar = calendar)
}

#' @export
#' @rdname pretty
setMethod("pretty", "RataDie", pretty.RataDie)

# Format =======================================================================
#' @export
#' @method format TimeScale
format.TimeScale <- function(x, ...) {
  msg <- sprintf("%s %s", calendar_unit(x), calendar_label(x))
  trimws(msg)
}

#' @export
#' @rdname format
setMethod("format", "TimeScale", format.TimeScale)

#' @export
#' @method format RataDie
format.RataDie <- function(x, prefix = c("a", "ka", "Ma", "Ga"), label = TRUE,
                           calendar = getOption("aion.calendar"), ...) {
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
  trimws(sprintf("%g%s%s", y / power, prefix, label))
}

#' @export
#' @rdname format
setMethod("format", "RataDie", format.RataDie)

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
      msg <- tr_("%s%s counted forwards from %g.")
    } else {
      msg <- tr_("%s%s counted backwards from %g.")
    }
    msg <- sprintf(msg, era, calendar_unit(object), calendar_epoch(object))
    cat(trimws(msg), sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "RataDie",
  definition = function(object) {
    msg <- tr_("Rata die: number of days since 01-01-01 (Gregorian).")
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
    msg <- tr_("%d x %d x %d time series observed between %s and %s r.d.")
    msg <- sprintf(msg, n[1L], n[2L], n[3L], start, end)
    cat(msg, sep = "\n")
  }
)

setMethod(
  f = "show",
  signature = "TimeIntervals",
  definition = function(object) {
    n <- length(object)
    start <- format(min(start(object)))
    end <- format(max(end(object)))
    msg <- tr_("%d time intervals observed between %s and %s r.d.")
    msg <- sprintf(msg, n, start, end)
    cat(msg, sep = "\n")
  }
)
