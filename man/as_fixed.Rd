% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/coerce.R
\docType{methods}
\name{as_fixed}
\alias{as_fixed}
\alias{as_fixed-method}
\alias{as_fixed,numeric-method}
\title{Coerce to \emph{Rata Die}}
\usage{
as_fixed(from)

\S4method{as_fixed}{numeric}(from)
}
\arguments{
\item{from}{A \code{\link{numeric}} vector of \emph{rata die}.}
}
\value{
A \code{\linkS4class{RataDie}} object.
}
\description{
Coerce to \emph{Rata Die}
}
\examples{
## R 1.0.0
(y <- fixed(year = 2000, month = 02, day = 29, calendar = CE()))
as_date(y, calendar = CE())
as_year(y, calendar = CE())

## Create a vector of years BP (Gregorian)
## (every two years starting from 2000 BP)
(years <- seq(from = 2000, by = -2, length.out = 10))
## Convert years to rata die
(rd <- fixed(years, calendar = BP()))
## Convert back to Gregorian years BP
as_year(rd, calendar = BP())

## More convenient
(rd <- fixed_from_BP(years))
fixed_to_BP(rd)
}
\references{
Reingold, E. M. and Dershowitz, N. (2018). \emph{Calendrical Calculations:
The Ultimate Edition}. Cambridge University Press.
\doi{10.1017/9781107415058}.
}
\seealso{
Other fixed date tools: 
\code{\link{arithmetic}},
\code{\link{as_date}()},
\code{\link{as_decimal}()},
\code{\link{as_year}()},
\code{\link{fixed}()},
\code{\link{fixed_gregorian}},
\code{\link{fixed_julian}},
\code{\link{format}()},
\code{\link{pretty}()}
}
\author{
N. Frerebeau
}
\concept{fixed date tools}
