# FORMAT

# Format =======================================================================
#' @export
#' @method format TimeIntervals
format.TimeIntervals <- function(x, calendar = get_calendar(), ...) {
  debut <- start(x, calendar = calendar)
  fin <- end(x, calendar = calendar)

  msg <- sprintf("[%g, %g]", debut, fin)
  trimws(msg)
}

#' @export
#' @rdname format
setMethod("format", "TimeIntervals", format.TimeIntervals)

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
                           calendar = get_calendar(), ...) {
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
