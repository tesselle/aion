# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# Time Scales ==================================================================
setValidity(
  Class = "TimeScale",
  method = function(object) {
    ## Get data
    era_label <- object@era_label
    era_name <- object@era_name
    era_epoch <- object@era_epoch
    era_scale <- object@era_scale
    era_direction <- object@era_direction
    era_unit <- object@era_unit
    era_days <- object@era_days

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_scalar(era_label, "character")),
      arkhe::validate(arkhe::assert_scalar(era_name, "character")),
      arkhe::validate(arkhe::assert_scalar(era_epoch, "numeric")),
      arkhe::validate(arkhe::assert_scalar(era_scale, "integer")),
      arkhe::validate(arkhe::assert_scalar(era_direction, "integer")),
      arkhe::validate(arkhe::assert_scalar(era_unit, "character")),
      arkhe::validate(arkhe::assert_scalar(era_days, "numeric"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

# Time Series ==================================================================
setValidity(
  Class = "TimeSeries",
  method = function(object) {
    ## Get data
    time_labels <- object@time_labels
    time_start <- object@time_start
    time_frequency <- object@time_frequency
    p <- ncol(object)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_type(object, "numeric")),
      arkhe::validate(arkhe::assert_type(time_labels, "character")),
      arkhe::validate(arkhe::assert_length(time_labels, p)),
      arkhe::validate(arkhe::assert_scalar(time_start, "numeric")),
      arkhe::validate(arkhe::assert_scalar(time_frequency, "numeric"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)
