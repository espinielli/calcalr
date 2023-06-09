% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/egyptian_calendar.R
\name{fixed_from_egyptian}
\alias{fixed_from_egyptian}
\title{Return the fixed date corresponding to Egyptian date \code{e_date}}
\usage{
fixed_from_egyptian(e_date)
}
\arguments{
\item{e_date}{An Egyptian calendar date.}
}
\value{
A Rata Die.
}
\description{
Return the fixed date corresponding to Egyptian date \code{e_date}
}
\examples{
\dontrun{
fixed_from_egyptian(egyptian_date(1, 2, 3))
}
}
