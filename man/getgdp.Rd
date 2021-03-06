% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BEAData_utilities.r
\name{getgdp}
\alias{getgdp}
\title{Get a data frame with real gdp, nominal gdp and the gdp price index, annual or quarterly.}
\usage{
getgdp(gfreq = "A")
}
\arguments{
\item{gfreq}{"A" (annual) or "Q" (quarterly). Defaults to "A".}
}
\value{
data frame with:

   year (integer), rgdp, gdp, gdppi (double), if freq=="A"

   date (date), rgdp, gdp, gdppi (double) if freq=="Q"
}
\description{
Get a data frame with real gdp, nominal gdp and the gdp price index, annual or quarterly.
}
\examples{
library("tidyverse")
getgdp()
getgdp("A")
getgdp("Q")
}
