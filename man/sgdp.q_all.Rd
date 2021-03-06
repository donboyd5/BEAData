% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sgdp.q_all.r
\docType{data}
\name{sgdp.q_all}
\alias{sgdp.q_all}
\title{BEA Regional GDP data all components, quarterly.}
\format{
Data frame with 1 row per industry per component per quarter.
\describe{
\item{stabbr}{state abbreviation, factor}
\item{date}{First day of quarter, date}
\item{component}{BEA numeric code, integer}
\item{compname}{BEA name, character}
\item{ind}{BEA industry code, integer}
\item{indclass}{BEA industry classification, character}
\item{indname}{BEA industry name, character}
\item{value}{Amount, numeric}
}
}
\source{
Bureau of Economic Analysis
}
\usage{
sgdp.q_all
}
\description{
Regional, quarterly all components, various years.
}
\examples{
  sgdp.q_all
}
\keyword{datasets}
