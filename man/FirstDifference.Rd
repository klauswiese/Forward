\name{FirstDifference}
\alias{FirstDifference}

\title{
First Difference
}
\description{
Find the First Difference for a time series}
\usage{
FirstDifference(r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{A raster stack}
  \item{...}{Options from the calc function from raster package}
}

\value{Calculate the first diffence from a raster time series}
\references{}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#Set variables
ACF <- stack("ACF.tiff")

FirstDifference(ACF)
}

