\name{monthlyMODIS}
\alias{monthlyMODIS}

\title{
monthly MODIS
}
\description{
Calculates the monthly mean of measures made by NDVI 8 days MODIS product}
\usage{
monthlyMODIS(MODISyear, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MODISyear}{Time series of 8 days products of MODIS, a raster stack}
  \item{year}{The year of interest, a numeric value}
}

\value{A 12 layers raster stack of an 8 days MODIS product}
\references{}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
NDVI_2001 <- stack("NDVI_2001.tiff")

monthlyMODIS(NDVI_2001, 2001)
}

