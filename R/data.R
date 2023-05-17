#' Sample Data from Reingold and Dershowitz (2018)
#'
#' A dataset of 33 dates from the years -1000 to 2100 with their equivalents on
#' differents calendars.
#' @format A [`data.frame`] with 33 rows and 14 variables:
#'  \describe{
#'   \item{`rata_die`}{Rata die.}
#'   \item{`weekday`}{Week day.}
#'   \item{`jd`}{Julian day.}
#'   \item{`mjd`}{Modified Julian day.}
#'   \item{`unix`}{Unix.}
#'   \item{`gregorian_year`, `gregorian_month`, `gregorian_day`}{Gregorian date.}
#'   \item{`julian_year`, `julian_month`, `julian_day`}{Julian date.}
#'   \item{`egyptian_year`, `egyptian_month`, `egyptian_day`}{Egyptian date.}
#'  }
#' @references
#'  Reingold, E. M. and Dershowitz, N. (2018). *Calendrical Calculations:
#'  The Ultimate Edition*. Cambridge University Press.
#'  \doi{10.1017/9781107415058}.
#' @family datasets
#' @keywords datasets
"dates"
