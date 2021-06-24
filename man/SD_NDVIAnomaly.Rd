\name{SD_NDVIAnomaly}
\alias{SD_NDVIAnomaly}

\title{
Standardised NDVI Anomaly
}
\description{
Estimate Standardised NDVI Anomaly}
\usage{
SD_NDVIAnomaly(NDVI, YearLength)
}

\arguments{
  \item{NDVI}{NDVI index raster time series}
  \item{YearLength}{The number of layer represent a complete year}
}

\value{The yearly standardised anomaly for NDVI time series}
\references{}
\author{Klaus W. Wiese Acosta}


\examples{
#Set variables
NDVI <- stack("NDVITimeSeries.tiff")

SD_NDVIAnomaly(NDVI, 46)
}

