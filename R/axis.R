# AXIS

# Pretty =======================================================================
#' @export
#' @method pretty RataDie
pretty.RataDie <- function(x, calendar = getOption("aion.calendar"), ...) {
  if (is.null(calendar)) return(pretty(as.numeric(x), ...))

  x <- as_year(x, calendar = calendar, decimal = FALSE)
  x <- pretty(x, ...)
  if (methods::is(calendar, "JulianCalendar")) x[x == 0] <- 1
  fixed(year = x, calendar = calendar)
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

# Axis =========================================================================
#' @export
#' @rdname year_axis
year_axis <- function(side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                      labels = TRUE, calendar = getOption("aion.last_calendar"),
                      current_calendar = getOption("aion.last_calendar"),
                      ...) {
  no_at <- missing(at) || is.null(at) || !is.numeric(at)
  if (no_at) at <- graphics::axTicks(side = side)

  if (!is.logical(labels)) {
    labels <- labels[keep]
  } else if (isTRUE(labels)) {
    ## If last_calendar is NULL, then the last plot was expressed in rata die
    if (is.null(current_calendar)) {
      at <- as_fixed(at)
    } else {
      if (methods::is(calendar, "JulianCalendar")) at[at == 0] <- 1
      at <- fixed(at, calendar = current_calendar)
    }
    if (!is.null(calendar)) {
      at <- pretty(at, calendar = calendar)
      labels <- format(at, prefix = format, label = FALSE, calendar = calendar)
      if (!is.null(current_calendar)) at <- as_year(at, calendar = current_calendar)
    }
  } else if (isFALSE(labels)) {
    labels <- rep("", length(at))
  }

  graphics::axis(side, at = as.numeric(at), labels = labels, ...)
}
