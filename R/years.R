# DECIMAL YEAR
#' @include AllGenerics.R
NULL

#' @export
#' @rdname as_decimal
#' @aliases as_decimal,numeric,numeric,numeric,TimeScale-method
setMethod(
  f = "as_decimal",
  signature = c(year = "numeric", month = "numeric", day = "numeric", calendar = "TimeScale"),
  definition = function(year, month, day, calendar) {
    ## Switch origin
    year <- (year - calendar_epoch(calendar)) * calendar_direction(calendar)

    ## Year length in days
    start <- fixed(year, 01, 01, calendar = calendar)
    end <- fixed(year, 12, 31, calendar = calendar)
    total <- end - start + 1

    ## Elapsed time
    date <- fixed(year, month, day, calendar = calendar)
    sofar <- date - start

    unclass(year + sofar / total)
  }
)
