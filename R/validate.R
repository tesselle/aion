# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# TimeScale =====================================================================
setValidity(
  Class = "TimeScale",
  method = function(object) {
    ## Get data
    label <- object@label
    name <- object@name
    epoch <- object@epoch
    fixed <- object@fixed
    direction <- object@direction
    year <- object@year

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_length(label, 1)),
      arkhe::validate(arkhe::assert_length(name, 1)),
      arkhe::validate(arkhe::assert_length(epoch, 1)),
      arkhe::validate(arkhe::assert_length(fixed, 1)),
      arkhe::validate(arkhe::assert_length(direction, 1)),
      arkhe::validate(arkhe::assert_length(year, 1))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

# Time Series ==================================================================
# setValidity(
#   Class = "RataDie",
#   method = function(object) {
#
#   }
# )

setValidity(
  Class = "TimeSeries",
  method = function(object) {
    ## Get data
    time <- object@.Time
    m <- nrow(object)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_type(object, "numeric")),
      arkhe::validate(arkhe::assert_length(time, m)),
      arkhe::validate(arkhe::assert_infinite(time))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

setValidity(
  Class = "TimeIntervals",
  method = function(object) {
    ## Get data
    names <- object@.Id
    start <- object@.Start
    end <- object@.End
    m <- length(start)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_length(names, m)),
      arkhe::validate(arkhe::assert_length(end, m)),
      arkhe::validate(arkhe::assert_missing(names)),
      arkhe::validate(arkhe::assert_missing(start)),
      arkhe::validate(arkhe::assert_missing(end)),
      arkhe::validate(assert_ordered(start, end))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

assert_ordered <- function(start, end) {
  arg_start <- deparse(substitute(start))
  arg_end <- deparse(substitute(end))
  if (any(start > end)) {
    msg <- sprintf(tr_("%s is later than %s."), sQuote(arg_start), sQuote(arg_end))
    stop(msg)
  }
  invisible(NULL)
}
