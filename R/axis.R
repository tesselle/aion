# AXIS

# Pretty =======================================================================
#' @export
#' @method pretty RataDie
pretty.RataDie <- function(x, calendar = get_calendar(), ...) {
  if (is.null(calendar)) return(pretty(as.numeric(x), ...))

  x <- as_year(x, calendar = calendar, decimal = FALSE)
  x <- pretty(x, ...)
  if (methods::is(calendar, "JulianCalendar")) x[x == 0] <- 1
  fixed(year = x, calendar = calendar)
}

#' @export
#' @rdname pretty
setMethod("pretty", "RataDie", pretty.RataDie)

# Axis =========================================================================
#' @export
#' @rdname year_axis
year_axis <- function(side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                      labels = TRUE, calendar = get_calendar("current"),
                      current_calendar = get_calendar("current"),
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
