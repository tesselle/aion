.onLoad <- function(libname, pkgname) {
  op <- options()
  op.aion <- list(
    aion.verbose = interactive()
  )
  toset <- !(names(op.aion) %in% names(op))
  if(any(toset)) options(op.aion[toset])

  invisible()
}
