# PLOT
#' @include AllGenerics.R
NULL

# Plot =========================================================================
#' @export
#' @method plot TimeSeries
plot.TimeSeries <- function(x, facet = c("multiple", "single"),
                            calendar = getOption("aion.calendar"),
                            panel = graphics::lines, flip = FALSE, ncol = NULL,
                            main = NULL, sub = NULL,
                            ann = graphics::par("ann"), axes = TRUE,
                            frame.plot = axes,
                            panel.first = NULL, panel.last = NULL, ...) {
  ## Validation
  facet <- match.arg(facet, several.ok = FALSE)

  ## Save calendar for further use (e.g. year_axis() or year_grid())
  options(aion.last_calendar = calendar)

  n <- dim(x)[2L]

  if (facet == "multiple" && n > 1) {
    .plot_multiple(x, calendar = calendar, panel = panel, y_flip = flip,
                   n_col = ncol, main = main, sub = sub, ann = ann, axes = axes,
                   frame.plot = frame.plot, panel.first = panel.first,
                   panel.last = panel.last, ...)
  } else {
    .plot_single(x, calendar = calendar, panel = panel, main = main, sub = sub,
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

.plot_single <- function(x, calendar, panel = graphics::lines,
                         main = NULL, sub = NULL,
                         ann = graphics::par("ann"), axes = TRUE,
                         frame.plot = axes,
                         panel.first = NULL, panel.last = NULL, ...) {
  n_series <- dim(x)[2L]
  n_dim <- dim(x)[3L]

  ## Graphical parameters
  dots <- list(...)
  col <- make_par(dots, "col", n = n_series)
  bg <- make_par(dots, "bg", n = n_series)
  pch <- make_par(dots, "pch", n = n_series)
  cex <- make_par(dots, "cex", n = n_series)
  lwd <- make_par(dots, "lwd", n = n_series)
  lty <- make_par(dots, "lty", n = n_series)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- time(x, calendar = NULL)
  xlim <- range(years)
  ylim <- range(x)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  for (j in seq_len(n_series)) {
    for (k in seq_len(n_dim)) {
      params <- c("col", "bg", "pch", "cex", "lwd", "lty")
      dots[params] <- list(col[j], bg[j], pch[j], cex[j], lwd[j], lty[j])
      args <- c(list(x = years, y = x[, j, k, drop = TRUE]), dots)
      do.call(panel, args)
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    year_axis(x = years, side = 1, format = TRUE, calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }
}

.plot_multiple <- function(x, calendar, panel = graphics::lines,
                           y_flip = TRUE, n_col = NULL,
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
  ylabs <- colnames(x) %||% paste("Series", n_seq, sep = " ")

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (y_flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(n_row, n_col)
  )
  on.exit(graphics::par(old_par))

  dots <- list(...)
  col <- make_par(dots, "col", n = m)
  pch <- make_par(dots, "pch", n = m)
  bg <- make_par(dots, "bg", n = m)
  cex <- make_par(dots, "cex", n = m)
  lwd <- make_par(dots, "lwd", n = m)
  lty <- make_par(dots, "lty", n = m)

  cex.lab <- dots$cex.lab %||% graphics::par("cex.lab")
  col.lab <- dots$col.lab %||% graphics::par("col.lab")
  font.lab <- dots$font.lab %||% graphics::par("font.lab")
  cex.axis <- dots$cex.axis %||% graphics::par("cex.axis")
  col.axis <- dots$col.axis %||% graphics::par("col.axis")
  font.axis <- dots$font.axis %||% graphics::par("font.axis")
  cex.main <- dots$cex.main %||% graphics::par("cex.main")
  font.main <- dots$font.main %||% graphics::par("font.main")
  col.main <- dots$col.main %||% graphics::par("col.main")

  years <- time(x, calendar = NULL)
  xlim <- range(years)
  for (j in n_seq) {
    xi <- x[, j, , drop = FALSE]

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    ylim <- range(xi)
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    for (k in m_seq) {
      params <- c("col", "bg", "pch", "cex", "lwd", "lty")
      dots[params] <- list(col[k], bg[k], pch[k], cex[k], lwd[k], lty[k])
      args <- c(list(x = years, y = x[, j, k, drop = TRUE]), dots)
      do.call(panel, args)
    }

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    do_x <- j %% n_row == 0 || j == n
    y_side <- if (j %% 2 || !y_flip) 2 else 4
    if (axes) {
      if (do_x) {
        year_axis(x = years, side = 1, format = TRUE, calendar = calendar,
                  xpd = NA, cex.axis = cex.axis,
                  col.axis = col.axis, font.axis = font.axis)
      }
      graphics::axis(side = y_side, xpd = NA, cex.axis = cex.axis,
                     col.axis = col.axis, font.axis = font.axis, las = 1)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        xlab <- if (is.null(calendar)) expression(italic("rata die")) else format(calendar)
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
}

# Image ========================================================================
#' @export
#' @method image TimeSeries
image.TimeSeries <- function(x, calendar = getOption("aion.calendar"), k = 1, ...) {
  ## Save calendar for further use (e.g. year_axis() or year_grid())
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
  graphics::image(x = years, y = n, z = x[, , k, drop = TRUE],
                  xlab = format(calendar), ylab = "",
                  xaxt = "n", yaxt = "n", ...)

  ## Construct axes
  year_axis(x = years, side = 1, format = TRUE, calendar = calendar,
            xpd = NA, cex.axis = cex.axis,
            col.axis = col.axis, font.axis = font.axis)
  graphics::axis(side = 2, at = n, labels = samples,
                 xpd = NA, cex.axis = cex.axis, las = 1,
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
#' @aliases year_axis,RataDie-method
setMethod(
  f = "year_axis",
  signature = c(x = "RataDie"),
  definition = function(x, side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                        labels = TRUE, calendar = getOption("aion.last_calendar"),
                        ...) {

    range <- sort(graphics::par("usr")[if (side %% 2) 1L:2L else 3L:4L])
    range[1L] <- ceiling(range[1L])
    range[2L] <- floor(range[2L])

    has_at <- !missing(at) && !is.null(at)
    if (has_at && is.numeric(at)) {
      x <- fixed(year = at, calendar = calendar)
    }

    x <- pretty(x, calendar = calendar)
    keep <- x >= range[1L] & x <= range[2L]
    x <- x[keep]

    if (!is.logical(labels))
      labels <- labels[keep]
    else if (isTRUE(labels))
      labels <- format(x, format = format, label = FALSE, calendar = calendar)
    else if (isFALSE(labels))
      labels <- rep("", length(x))

    graphics::axis(side, at = x, labels = labels, ...)

    invisible(x)
  }
)

#' @export
#' @rdname year_axis
#' @aliases year_axis,TimeSeries-method
setMethod(
  f = "year_axis",
  signature = c(x = "TimeSeries"),
  definition = function(x, side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                        labels = TRUE, calendar = getOption("aion.last_calendar"),
                        ...) {
    x <- time(x, calendar = NULL)
    methods::callGeneric(x, side = side, at = at, format = format,
                         labels = labels, calendar = calendar, ...)
  }
)

# Grid =========================================================================
#' @export
#' @rdname year_grid
year_grid <- function(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
                      lwd = graphics::par("lwd"),
                      calendar = getOption("aion.last_calendar")) {

  if (is.null(nx) || (!is.na(nx) && nx >= 1)) {
    if (is.null(nx)) {
      U <- graphics::par("xaxp")
      nx <- U[3L]
    } else {
      U <- graphics::par("usr")
    }
    at <- pretty(as_fixed(c(U[1L], U[2L])), calendar = calendar, n = nx)
    graphics::abline(v = at, col = col, lty = lty, lwd = lwd)
  }
  if (is.null(ny) || (!is.na(ny) && ny >= 1)) {
    if (is.null(ny)) {
      ax <- graphics::par("yaxp")
      at <- graphics::axTicks(2, axp = ax)
    }
    else {
      U <- graphics::par("usr")
      at <- pretty(c(U[3L], U[4L]), n = ny)
    }
    graphics::abline(h = at, col = col, lty = lty, lwd = lwd)
  }
}
