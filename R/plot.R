# PLOT
#' @include AllGenerics.R
NULL

# Plot =========================================================================
#' @export
#' @method plot TimeSeries
plot.TimeSeries <- function(x, type = c("multiple", "single"),
                            calendar = getOption("aion.calendar"),
                            panel = graphics::lines, flip = FALSE, ncol = NULL,
                            main = NULL, sub = NULL,
                            ann = graphics::par("ann"), axes = TRUE,
                            frame.plot = axes,
                            panel.first = NULL, panel.last = NULL, ...) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)
  n_series <- NCOL(x)
  seq_series <- seq_len(n_series)

  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  if (length(col) != n_series) col <- rep(col, length.out = n_series)
  if (length(lty) != n_series) lty <- rep(lty, length.out = n_series)
  if (length(lwd) != n_series) lwd <- rep(lwd, length.out = n_series)

  if (type == "multiple" && n_series > 1) {
    .plot_stacked(x, calendar = calendar, panel = panel, y_flip = flip,
                  n_col = ncol, main = main, sub = sub, ann = ann, axes = axes,
                  frame.plot = frame.plot, panel.first = panel.first,
                  panel.last = panel.last, ...)
  } else {
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
    for (i in seq_series) {
      graphics::lines(x = years, y = x[, i], col = col[i],
                      lty = lty[i], lwd = lwd[i])
    }

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    if (axes) {
      axis_year(x = years, side = 1, format = TRUE, calendar = calendar)
      graphics::axis(side = 2)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      xlab <- format(calendar)
      ylab <- NULL
      graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
  }

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,TimeSeries,missing-method
setMethod("plot", c(x = "TimeSeries", y = "missing"), plot.TimeSeries)


.plot_stacked <- function(x, calendar = getOption("aion.calendar"),
                          panel = graphics::lines, y_flip = TRUE, n_col = NULL,
                          main = NULL, sub = NULL,
                          ann = graphics::par("ann"), axes = TRUE,
                          frame.plot = axes,
                          panel.first = NULL, panel.last = NULL, ...) {

  panel <- match.fun(panel)
  n <- NCOL(x)
  n_seq <- seq_len(n)
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

  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")

  years <- time(x, calendar = NULL)
  xlim <- range(years)
  for (i in n_seq) {
    xi <- x[, i, drop = TRUE]

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
    panel(x = years, y = x[, i], ...)

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    do_x <- i %% n_row == 0 || i == n
    y_side <- if (i %% 2 || !y_flip) 2 else 4
    if (axes) {
      if (do_x) {
        axis_year(x = years, side = 1, format = TRUE, calendar = calendar,
                  xpd = NA, cex.axis = cex.axis,
                  col.axis = col.axis, font.axis = font.axis)
      }
      graphics::axis(side = y_side, xpd = NA, cex.axis = cex.axis,
                     col.axis = col.axis, font.axis = font.axis)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        xlab <- format(calendar)
        graphics::mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab,
                        font = font.lab)
      }
      graphics::mtext(ylabs[[i]], side = y_side, line = 3, cex = cex.lab,
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
image.TimeSeries <- function(x, calendar = getOption("aion.calendar"), ...) {
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
  graphics::image(x = years, y = n, z = x@.Data,
                  xlab = format(calendar), ylab = "",
                  xaxt = "n", yaxt = "n", ...)

  ## Construct axes
  axis_year(x = years, side = 1, format = TRUE, calendar = calendar,
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
#' @rdname axis_year
#' @aliases axis_year,RataDie-method
setMethod(
  f = "axis_year",
  signature = c(x = "RataDie"),
  definition = function(x, side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                        labels = TRUE, calendar = getOption("aion.calendar"),
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
#' @rdname axis_year
#' @aliases axis_year,TimeSeries-method
setMethod(
  f = "axis_year",
  signature = c(x = "TimeSeries"),
  definition = function(x, side, at = NULL, format = c("a", "ka", "Ma", "Ga"),
                        labels = TRUE, calendar = getOption("aion.calendar"),
                        ...) {
    x <- time(x, calendar = NULL)
    methods::callGeneric(x, side = side, at = at, format = format,
                         labels = labels, calendar = calendar, ...)
  }
)
