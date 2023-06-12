# PLOT
#' @include AllGenerics.R
NULL

# Plot =========================================================================
#' @export
#' @method plot TimeSeries
plot.TimeSeries <- function(x, facet = c("multiple", "single"),
                            calendar = getOption("aion.calendar"),
                            panel = graphics::lines, flip = FALSE, ncol = NULL,
                            xlab = NULL, ylab = NULL,
                            main = NULL, sub = NULL,
                            ann = graphics::par("ann"), axes = TRUE,
                            frame.plot = axes,
                            panel.first = NULL, panel.last = NULL, ...) {
  ## Validation
  facet <- match.arg(facet, several.ok = FALSE)

  ## Save calendar for further use, e.g. year_axis()
  options(aion.last_calendar = calendar)

  n <- dim(x)[2L]

  if (facet == "multiple" && n > 1) {
    .plot_multiple(x, calendar = calendar, panel = panel, y_flip = flip,
                   n_col = ncol, xlab = xlab, ylab = ylab,
                   main = main, sub = sub, ann = ann, axes = axes,
                   frame.plot = frame.plot, panel.first = panel.first,
                   panel.last = panel.last, ...)
  } else {
    .plot_single(x, calendar = calendar, panel = panel,
                 xlab = xlab, ylab = ylab,
                 main = main, sub = sub,
                 ann = ann, axes = axes,
                 frame.plot = frame.plot, panel.first = panel.first,
                 panel.last = panel.last, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,TimeSeries,missing-method
setMethod("plot", c(x = "TimeSeries", y = "missing"), plot.TimeSeries)

#' Single Panel Plot
#'
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param panel A [`function`] in the form `function(x, y, ...)`
#'  which gives the action to be carried out in each panel of the display.
#'  The default is [graphics::lines()].
#' @param xlim,ylim A length-two [`numeric`] vector specifying the the x and y
#'  limits.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further parameters to be passed to `panel`
#'  (e.g. [graphical parameters][graphics::par]).
#' @return
#'  Called for its side-effects: it results in a graphic being displayed.
#'  Invisibly returns `x`.
#' @keywords internal
#' @noRd
.plot_single <- function(x, calendar, panel = graphics::lines,
                         xlab = NULL, ylab = NULL,
                         xlim = NULL, ylim = NULL,
                         main = NULL, sub = NULL,
                         ann = graphics::par("ann"), axes = TRUE,
                         frame.plot = axes,
                         panel.first = NULL, panel.last = NULL, ...) {
  n_series <- dim(x)[2L]
  n_dim <- dim(x)[3L]

  ## Graphical parameters
  dots <- list(...)
  col <- make_par(dots, "col", n = n_dim)
  bg <- make_par(dots, "bg", n = n_dim)
  pch <- make_par(dots, "pch", n = n_dim)
  cex <- make_par(dots, "cex", n = n_dim)
  lwd <- make_par(dots, "lwd", n = n_dim)
  lty <- make_par(dots, "lty", n = n_dim)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- time(x, calendar = calendar)
  xlim <- xlim %||% xlim(x, calendar = calendar)
  ylim <- ylim %||% range(x)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  for (j in seq_len(n_series)) {
    for (k in seq_len(n_dim)) {
      params <- list(col = col[k], bg = bg[k], pch = pch[k],
                     cex = cex[k], lwd = lwd[k], lty = lty[k])
      dots <- utils::modifyList(dots, params)
      args <- c(list(x = years, y = x[, j = j, k = k, drop = TRUE]), dots)
      do.call(panel, args)
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    year_axis(side = 1, format = TRUE, calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    cal_lab <- if (is.null(calendar)) expression(italic("rata die")) else format(calendar)
    xlab <- xlab %||% cal_lab
    # ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' Multiple Panels Plot
#'
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @param panel A [`function`] in the form `function(x, y, ...)`
#'  which gives the action to be carried out in each panel of the display.
#'  The default is [graphics::lines()].
#' @param y_flip A [`logical`] scalar: should the y-axis (ticks and numbering)
#'  be flipped from side 2 (left) to 4 (right) from series to series?
#' @param ncol An [`integer`] specifying the number of columns to use.
#'  Defaults to 1 for up to 4 series, otherwise to 2.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further parameters to be passed to `panel`
#'  (e.g. [graphical parameters][graphics::par]).
#' @return
#'  Called for its side-effects: it results in a graphic being displayed.
#'  Invisibly returns `x`.
#' @keywords internal
#' @noRd
.plot_multiple <- function(x, calendar, panel = graphics::lines,
                           y_flip = TRUE, n_col = NULL,
                           xlab = NULL, ylab = NULL,
                           main = NULL, sub = NULL,
                           ann = graphics::par("ann"), axes = TRUE,
                           frame.plot = axes,
                           panel.first = NULL, panel.last = NULL, ...) {

  panel <- match.fun(panel)
  n <- dim(x)[2L]
  m <- dim(x)[3L]
  n_seq <- seq_len(n)
  m_seq <- seq_len(m)
  if (is.null(n_col)) n_col <- if (n > 4) 2 else 1
  n_row <- ceiling(n / n_col)

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (y_flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(n_row, n_col)
  )
  on.exit(graphics::par(old_par))

  dots <- list(...)
  cex.lab <- make_par(dots, "cex.lab")
  col.lab <- make_par(dots, "col.lab")
  font.lab <- make_par(dots, "font.lab")
  cex.axis <- make_par(dots, "cex.axis")
  col.axis <- make_par(dots, "col.axis")
  font.axis <- make_par(dots, "font.axis")
  cex.main <- make_par(dots, "cex.main")
  font.main <- make_par(dots, "font.main")
  col.main <- make_par(dots, "col.main")

  years <- time(x, calendar = calendar)
  xlim <- xlim(x, calendar = calendar)
  ylabs <- ylab %||% colnames(x) %||% paste("Series", n_seq, sep = " ")
  for (j in n_seq) {
    ## Plot
    xj <- x[, j, , drop = FALSE]
    .plot_single(xj, calendar = calendar, panel = panel,
                 xlim = xlim, ylim = NULL,
                 main = NULL, sub = NULL, ann = FALSE, axes = FALSE,
                 frame.plot = frame.plot,
                 panel.first = panel.first, panel.last = panel.last, ...)

    ## Construct Axis
    xaxt <- make_par(dots, "xaxt")
    yaxt <- make_par(dots, "yaxt")
    do_x <- (j %% n_row == 0 || j == n)
    y_side <- if (j %% 2 || !y_flip) 2 else 4
    if (axes) {
      if (do_x && xaxt != "n") {
        year_axis(side = 1, format = TRUE, calendar = calendar,
                  xpd = NA, cex.axis = cex.axis,
                  col.axis = col.axis, font.axis = font.axis)
      }
      if (yaxt != "n") {
        graphics::axis(side = y_side, xpd = NA, cex.axis = cex.axis,
                       col.axis = col.axis, font.axis = font.axis, las = 1)
      }
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        cal_lab <- if (is.null(calendar)) expression(italic("rata die")) else format(calendar)
        xlab <- xlab %||% cal_lab
        graphics::mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab,
                        font = font.lab)
      }
      graphics::mtext(ylabs[[j]], side = y_side, line = 3, cex = cex.lab,
                      col = col.lab, font = font.lab)
    }
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 3, cex = cex.main, font = font.main,
                    col = col.main)
  }

  invisible(x)
}

#' Compute x Limits
#'
#' Computes x limits for a time series according to a given calendar.
#' This ensures that the x axis is always in chronological order.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object.
#' @return A length-two [`numeric`] vector.
#' @keywords internal
#' @noRd
xlim <- function(x, calendar) {
  x <- range(time(x, calendar = NULL))
  if (is.null(calendar)) return(x)
  as_year(x, calendar = calendar)
}

# Image ========================================================================
#' @export
#' @method image TimeSeries
image.TimeSeries <- function(x, calendar = getOption("aion.calendar"), k = 1, ...) {
  ## Save calendar for further use, e.g. year_axis()
  options(aion.last_calendar = calendar)

  ## Get data
  n <- seq_len(NCOL(x))
  samples <- colnames(x) %||% paste0("S1", n)
  years <- time(x, calendar = NULL)

  ## Graphical parameters
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

  ## Save and restore
  mar <- graphics::par("mar")
  mar[2] <- inch2line(samples, cex = cex.axis) + 0.5
  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  ## Plot
  z <- x[, , k = k, drop = TRUE]
  graphics::image(x = years, y = n, z = z,
                  xlab = format(calendar), ylab = "",
                  xaxt = "n", yaxt = "n", ...)

  ## Construct axes
  at <- as_fixed(graphics::axTicks(side = 1))
  at <- pretty(at, calendar = calendar)
  lab <- format(at, label = FALSE, calendar = calendar)
  graphics::axis(side = 1, at = at, labels = lab)
  graphics::axis(side = 2, at = n, labels = samples,
                 cex.axis = cex.axis, las = 1,
                 col.axis = col.axis, font.axis = font.axis)

  invisible(x)
}

#' @export
#' @rdname image
#' @aliases image,TimeSeries-method
setMethod("image", c(x = "TimeSeries"), image.TimeSeries)

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
      at <- fixed(at, calendar = current_calendar)
    }
    if (!is.null(calendar)) {
      at <- pretty(at, calendar = calendar)
      labels <- format(at, format = format, label = FALSE, calendar = calendar)
      if (!is.null(current_calendar)) at <- as_year(at, calendar = current_calendar)
    }
  } else if (isFALSE(labels)) {
    labels <- rep("", length(at))
  }

  graphics::axis(side, at = as.numeric(at), labels = labels, ...)
}

