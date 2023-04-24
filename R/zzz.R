.onLoad <- function(libname, pkgname) {
  op <- options()
  op.chronos <- list(
    chronos.precision = 1
  )
  toset <- !(names(op.chronos) %in% names(op))
  if(any(toset)) options(op.chronos[toset])

  invisible()
}
