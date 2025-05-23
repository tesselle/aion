% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{TimeIntervals-class}
\alias{TimeIntervals-class}
\alias{.TimeIntervals}
\title{TimeIntervals}
\description{
An S4 class to represent time intervals.
}
\section{Slots}{

\describe{
\item{\code{.Id}}{A \code{\link{character}} vector specifying the identifier/name of intervals.
Duplicated values are interpreted as disjoint intervals referring to the
same event.}

\item{\code{.Start}}{A \code{\linkS4class{RataDie}} object giving the start time of the
intervals.}

\item{\code{.End}}{A \code{\linkS4class{RataDie}} object giving the end time of the intervals.}
}}

\seealso{
Other classes: 
\code{\link{GregorianCalendar-class}},
\code{\link{JulianCalendar-class}},
\code{\link{RataDie-class}},
\code{\link{TimeScale-class}},
\code{\link{TimeSeries-class}}

Other time classes: 
\code{\link{RataDie-class}},
\code{\link{TimeSeries-class}}
}
\author{
N. Frerebeau
}
\concept{classes}
\concept{time classes}
