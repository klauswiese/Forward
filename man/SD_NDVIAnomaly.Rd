\name{SD_NDVIAnomaly}
\alias{SD_NDVIAnomaly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardised NDVI Anomaly
}
\description{
Estimate Standardised NDVI Anomaly}
\usage{
SD_NDVIAnomaly(NDVI, YearLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NDVI}{NDVI index raster time series}
  \item{YearLength}{The number of layer represent a complete year}
}

\value{The yearly standardised anomaly for NDVI time series}
\references{}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
NDVI <- stack("NDVITimeSeries.tiff")

SD_NDVIAnomaly(NDVI, 46)
}

