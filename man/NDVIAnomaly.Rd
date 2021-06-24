\name{NDVIAnomaly}
\alias{NDVIAnomaly}

\title{
NDVI Anomaly
}
\description{
Estimate NDVI anomaly}
\usage{
NDVIAnomaly(NDVI, YearLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NDVI}{NDVI index raster time series}
  \item{YearLength}{The number of layer represent a complete year}
}

\value{The yearly anomaly for NDVI time series}
\references{}
\author{Klaus W. Wiese Acosta}


\examples{
#Set variables
NDVI <- stack("NDVITimeSeries.tiff")

NDVIanomaly(NDVI, 46)
}

