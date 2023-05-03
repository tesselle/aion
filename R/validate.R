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
    direction <- object@direction
    year <- object@year

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_scalar(label, "character")),
      arkhe::validate(arkhe::assert_scalar(name, "character")),
      arkhe::validate(arkhe::assert_scalar(epoch, "numeric")),
      arkhe::validate(arkhe::assert_scalar(direction, "integer")),
      arkhe::validate(arkhe::assert_scalar(year, "numeric"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

# Time Series ==================================================================
setValidity(
  Class = "TimeLine",
  method = function(object) {
    ## Get data
    scale <- object@scale

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_scalar(scale, "integer"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

setValidity(
  Class = "TimeSeries",
  method = function(object) {
    ## Get data
    time <- object@time
    m <- nrow(object)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_type(object, "numeric")),
      arkhe::validate(arkhe::assert_length(time, m))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)
