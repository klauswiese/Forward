#NDVI Anomaly
#K. Wiese July 2018
###################

SD_NDVIAnomaly <- function(NDVI, YearLength){
  
  years <- seq(1, dim(NDVI)[3], by=YearLength)
  
  lista <- list()
  for (i in 1:(dim(NDVI)[3]/YearLength)){
    lista[[i]] <- NDVI[[years[i]:(i*YearLength)]]
  }
  
  Medias <- lapply(lista, FUN = function(x) calc(x, mean))
  MediaTotal <- calc(NDVI, mean)
  sdTotal <- calc(NDVI, sd)
  
  SDAnomaly <- lapply(Medias, FUN = function(x) (x - MediaTotal)/sdTotal)
  
  return(stack(SDAnomaly))
}
