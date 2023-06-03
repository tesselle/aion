#' @details
#'  \tabular{ll}{
#'   **Package:** \tab chronos \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 1.0.0 \cr
#'   **License:** \tab GPL-3 \cr
#'  }
#'
#' @section Package options:
#'  `chronos` uses the following [options()] to configure behaviour:
#'  * `chronos.precision`: an [`integer`] indicating the number of decimal
#'    places (defaults to `NA`).
#'  * `chronos.calendar`: a [`TimeScale-class`] object (default calendar for
#'    printing).
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order):
#'
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'   Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#'   Joe Roe \tab *Universität Bern, Switzerland* \cr
#'  }
#'
#'  **Package maintainer**
#'
#'  Nicolas Frerebeau\cr
#'  \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#'  Archéosciences Bordeaux (UMR 6034)\cr
#'  Maison de l'Archéologie\cr
#'  Université Bordeaux Montaigne\cr
#'  F-33607 Pessac cedex\cr
#'  France
#' @name chronos-package
#' @aliases chronos
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @importFrom methods Arith as callGeneric callNextMethod is new setGeneric
#' setMethod setValidity .valueClassTest
NULL
