\name{Resilience}
\alias{Resilience}

\title{
Ecological Resilience and Resistence 
}
\description{
Estimate Ecological Resilience and Resistence using NDVI, Temperature and SPEI Anomalies}
\usage{
Resilience(NANOprev, TANO, SPEI, NANO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NANOprev}{NDVI_(t - 1) anomaly raster stack time series}
  \item{TANO}{Temperature anomaly raster stack time series}
  \item{SPEI}{The Standardised Precipitation-Evapotranspiration Index raster stack time series}
  \item{NANO}{NDVI_(t) anomaly raster stack time series}
}

\value{Resilience and Resistence to Temperature and Drought Index}
\references{De Keersmaecker, W., Lhermitte, S., Tits, L. et al.(2015). A model quantifying global vegetation resistance and resilience to short-term climate anomalies and their relationship with vegetation cover. Global Ecology and Biogeography. 24. 539–548.}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
NANOprev <- stack("NANOprev.tiff")
NANO <- stack("NANO.tiff")
TANO <- stack("TANO.tiff")
SPEI <- stack("SPEI.tiff")

Resilience(NANOprev = NANOprev, TANO = TANO, SPEI = SPEI, NANO = NANO)
}

