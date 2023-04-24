# PLOT
#' @include AllGenerics.R
NULL

time_label <- function(x) {
  paste("Year", era(x)@name, sep = " ")
}

#' @export
#' @method plot CalibratedAges
plot.CalibratedAges <- function(x, density = TRUE, interval = TRUE, level = 0.954,
                                warnings = TRUE, decreasing = TRUE,
                                xlim = NULL, main = NULL, sub = NULL,
                                ann = graphics::par("ann"), axes = TRUE,
                                frame.plot = FALSE,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  tcl <- list(...)$tcl %||% graphics::par("tcl")
  if (length(col) != nrow(x)) col <- rep(col, length.out = nrow(x))
  fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- years(x)
  xlim <- if (is.null(xlim)) c(years[[1]], years[[length(years)]]) else xlim
  ylim <- c(1, nrow(x) + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder
  mid <- median(x)
  k <- order(mid, decreasing = decreasing * x@calendar@direction == 1)
  x <- x[k, , drop = FALSE]
  col <- col[k]
  fill <- fill[k]

  ## Plot
  if (!density & !interval) density <- TRUE
  ages <- rev(seq_len(nrow(x)))
  # graphics::abline(h = ages, col = "grey")
  if (density) {
    for (i in ages) {
      d <- x[i, , drop = TRUE]

      d <- (d - min(d)) / max(d - min(d)) * 1.5
      k <- which(d > 0) # Keep only density > 0
      lb <- if (min(k) > 1) min(k) - 1 else min(k)
      ub <- if (max(k) < length(years)) max(k) + 1 else max(k)
      xi <- c(years[lb], years[k], years[ub])
      yi <- c(0, d[k], 0) + i

      graphics::polygon(xi, yi, border = NA, col = fill[i])
      graphics::lines(xi, yi, lty = "solid", col = "black")
    }
  }
  if (warnings) {
    status <- x@status
    graphics::text(x = xlim[1L], y = which(status == 1L), adj = c(0, 0),
                   labels = "Date is out of range.", col = "red")
    graphics::text(x = xlim[1L], y = which(status == 2L), adj = c(0, 0),
                   labels = "Date may extent out of range.", col = "red")
  }
  if (interval) {
    hpd <- hpdi(x, level = level)
    for (i in ages) {
      h <- hpd[[i]]
      graphics::segments(
        x0 = h[, "start"], x1 = h[, "stop"],
        y0 = i, y1 = i,
        col = if (density) "black" else col[i],
        lty = lty, lwd = lwd,
        lend = 1
      )
      graphics::segments(
        x0 = c(h[, "start"], h[, "stop"]), x1 = c(h[, "start"], h[, "stop"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = if (density) "black" else col[i],
        lty = lty, lwd = lwd,
        lend = 1
      )
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::mtext(names(x)[ages], side = 2, at = ages, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_label(x)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedAges,missing-method
setMethod("plot", c(x = "CalibratedAges", y = "missing"), plot.CalibratedAges)

#' @export
#' @method plot CalibratedSPD
plot.CalibratedSPD <- function(x, xlim = NULL, ylim = NULL,
                               main = NULL, sub = NULL, ann = graphics::par("ann"),
                               axes = TRUE, frame.plot = FALSE,
                               panel.first = NULL, panel.last = NULL, ...) {
  ## Graphical parameters
  col <- list(...)$col %||% c("grey")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- years(x)
  xlim <- if (is.null(xlim)) c(years[[1]], years[[length(years)]]) else xlim
  ylim <- if (is.null(ylim)) range(x) else ylim
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  xi <- c(years, rev(years))
  yi <- c(x, rep(0, length(x)))
  graphics::polygon(xi, yi, border = NA, col = col)
  graphics::lines(years, x, lty = "solid", col = "black")

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_label(x)
    ylab <- "Density"
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname c14_plot
#' @aliases plot,CalibratedSPD,missing-method
setMethod("plot", c(x = "CalibratedSPD", y = "missing"), plot.CalibratedSPD)
