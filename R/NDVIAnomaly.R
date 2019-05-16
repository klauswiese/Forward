#NDVI Anomaly
#K. Wiese July 2018
###################

NDVIAnomaly <- function(NDVI, YearLength){
  
  years <- seq(1, dim(NDVI)[3], by=YearLength)
  
  lista <- list()
  for (i in 1:(dim(NDVI)[3]/YearLength)){
    lista[[i]] <- NDVI[[years[i]:(i*YearLength)]]
  }
  
  Medias <- lapply(lista, FUN = function(x) calc(x, mean))
  MediaTotal <- calc(NDVI, mean)
  
  Anomaly <- lapply(Medias, FUN = function(x) x - MediaTotal)
  
  return(stack(Anomaly))
}
