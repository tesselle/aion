# OPERATORS
#' @include AllGenerics.R
NULL

# "+", "-", "*", "^", "%%", "%/%", "/"
#' @export
#' @rdname arithmetic
#' @aliases Arith,RataDie,RataDie-method
setMethod(
  f = "Arith",
  signature = c(e1 = "RataDie", e2 = "RataDie"),
  definition = function(e1, e2) {
    z <- methods::callGeneric(e1@.Data, e2@.Data)
    switch(
      .Generic[[1]],
      `+` = return(.RataDie(z)),
      `-` = return(.RataDie(z)),
      return(z)
    )
  }
)

# "==", ">", "<", "!=", "<=", ">="
#' @export
#' @rdname compare
#' @aliases Compare,RataDie,RataDie-method
setMethod(
  f = "Compare",
  signature = c(e1 = "RataDie", e2 = "RataDie"),
  definition = function(e1, e2) {
    methods::callGeneric(e1@.Data, e2@.Data)
  }
)
