.onLoad <- function(libname, pkgname) {
  op <- options()
  op.aion <- list(
    aion.precision = 1,
    aion.calendar = calendar("CE")
  )
  toset <- !(names(op.aion) %in% names(op))
  if(any(toset)) options(op.aion[toset])

  invisible()
}
