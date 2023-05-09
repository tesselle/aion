# OPERATORS
#' @include AllGenerics.R
NULL

# "+", "-", "*", "^", "%%", "%/%", "/"
#' @export
#' @rdname arithmetic
#' @aliases Arith,TimeLine,TimeLine-method
setMethod(
  f = "Arith",
  signature = c(e1 = "TimeLine", e2 = "TimeLine"),
  definition = function(e1, e2) {
    if (!identical(calendar(e1), calendar(e2))) {
      msg <- "You cannot perform operations involving different calendars."
      stop(msg, call. = FALSE)
    }

    x <- time(e1)
    y <- time(e2)
    z <- methods::callGeneric(e1 = x, e2 = y)

    switch(
      .Generic[[1]],
      `+` = return(methods::initialize(e1, z - calendar_epoch(e2))),
      `-` = return(methods::initialize(e1, z - calendar_epoch(e2))),
      return(x)
    )
  }
)

# "==", ">", "<", "!=", "<=", ">="
#' @export
#' @rdname compare
#' @aliases Compare,TimeLine,TimeLine-method
setMethod(
  f = "Compare",
  signature = c(e1 = "TimeLine", e2 = "TimeLine"),
  definition = function(e1, e2) {
    if (!identical(calendar(e1), calendar(e2))) {
      msg <- "You cannot perform operations involving different calendars."
      stop(msg, call. = FALSE)
    }

    x <- time(e1) * calendar_direction(e1)
    y <- time(e2) * calendar_direction(e2)
    methods::callGeneric(e1 = x, e2 = y)
  }
)
