\name{DifferenceAnnualCycle}
\alias{DifferenceAnnualCycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ACF Difference Annual Cycle
}
\description{
Calculate ACF Difference Annual Cycle, first lag minus the lag that represent the annual cycle}
\usage{
DifferenceAnnualCycle(ACF, ASL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ACF}{Time series ACF, a raster stack}
  \item{ASL}{Annual Season Length, a numeric value}
}

\value{A raster that represents the difference between the first lag ann the lag that represents the annual cycle, the closer the value to cero more stable time series}
\references{}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
ACF <- stack("ACF.tiff")

DifferenceAnnualCycle(ACF, 46)
}

