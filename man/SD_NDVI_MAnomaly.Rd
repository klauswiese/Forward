\name{SD_NDVI_MAnomaly}
\alias{SD_NDVI_MAnomaly}

\title{
Standardised NDVI Monthly Anomaly
}
\description{
Estimate Standardised NDVI Monthly Anomaly}
\usage{
SD_NDVI_MAnomaly(NDVI, YearLength, ini, fin)
}

\arguments{
  \item{NDVI}{NDVI index raster time series}
  \item{YearLength}{The number of layer represent a complete year}
  \item{ini}{First year of time series}
  \item{fin}{Last year of time series}
}

\value{The yearly standardised monthly anomaly for NDVI time series}
\references{}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
NDVI <- stack("NDVITimeSeries.tiff")

SD_NDVI_MAnomaly(NDVI, 12, 2001, 2006)
}

