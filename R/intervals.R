# TIME INTERVALS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname intervals
#' @aliases intervals,RataDie,RataDie,missing-method
setMethod(
  f = "intervals",
  signature = c(start = "RataDie", end = "RataDie", calendar = "missing"),
  definition = function(start, end, names = NULL) {
    n <- length(start)
    arkhe::assert_length(end, n)

    ## Set the names
    if (is.null(names)) {
      names <- paste0("I", seq_len(n))
    } else {
      names <- as.character(names)
    }
    arkhe::assert_length(names, n)

    .TimeIntervals(.Id = names, .Start = start, .End = end)
  }
)

#' @export
#' @rdname intervals
#' @aliases intervals,numeric,numeric,TimeScale-method
setMethod(
  f = "intervals",
  signature = c(start = "numeric", end = "numeric", calendar = "TimeScale"),
  definition = function(start, end, calendar, scale = 1, names = NULL) {
    ## Start
    if (methods::is(start, "RataDie")) {
      msg <- "%s is already expressed in rata die: %s is ignored."
      warning(sprintf(msg, sQuote("start"), sQuote("calendar")), call. = FALSE)
    } else {
      start <- fixed(start, calendar = calendar, scale = scale)
    }

    ## End
    if (methods::is(end, "RataDie")) {
      msg <- tr_("%s is already expressed in rata die: %s is ignored.")
      warning(sprintf(msg, sQuote("end"), sQuote("calendar")), call. = FALSE)
    } else {
      end <- fixed(end, calendar = calendar, scale = scale)
    }

    names <- names %||% names(start) %||% names(end)
    methods::callGeneric(start = start, end = end, names = names)
  }
)
