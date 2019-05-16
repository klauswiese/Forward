#NDVI monthly Anomaly
#K. Wiese July 2018
#####################

SD_NDVI_MAnomaly <- function(NDVI, YearLength, ini, fin){
  
  years <- seq(1, dim(NDVI)[3], by=YearLength)
  
  Meses <- list()
  for (i in 0:(12-1)){
    Meses[[i+1]] <- NDVI[[years+i]]
  }
  
  MediasAnuales <- lapply(Meses, FUN = function(x) calc(x, mean))
  sdAnuales <- lapply(Meses, FUN = function(x) calc(x, sd))
  
  Jan <- list()
  for (i in 1:dim(Meses[[1]])[3]){
    Jan[[i]] <- Meses[[1]][[i]]
  }
  
  Feb <- list()
  for (i in 1:dim(Meses[[2]])[3]){
    Feb[[i]] <- Meses[[2]][[i]]
  }
  
  Mar <- list()
  for (i in 1:dim(Meses[[3]])[3]){
    Mar[[i]] <- Meses[[3]][[i]]
  }
  
  Abr <- list()
  for (i in 1:dim(Meses[[3]])[3]){
    Abr[[i]] <- Meses[[4]][[i]]
  }
  
  May <- list()
  for (i in 1:dim(Meses[[5]])[3]){
    May[[i]] <- Meses[[5]][[i]]
  }
  
  Jun <- list()
  for (i in 1:dim(Meses[[6]])[3]){
    Jun[[i]] <- Meses[[6]][[i]]
  }
  
  Jul <- list()
  for (i in 1:dim(Meses[[7]])[3]){
    Jul[[i]] <- Meses[[7]][[i]]
  }
  
  Aug <- list()
  for (i in 1:dim(Meses[[8]])[3]){
    Aug[[i]] <- Meses[[8]][[i]]
  }
  
  Sep <- list()
  for (i in 1:dim(Meses[[9]])[3]){
    Sep[[i]] <- Meses[[9]][[i]]
  }
  
  Oct <- list()
  for (i in 1:dim(Meses[[10]])[3]){
    Oct[[i]] <- Meses[[10]][[i]]
  }
  
  Nov <- list()
  for (i in 1:dim(Meses[[11]])[3]){
    Nov[[i]] <- Meses[[11]][[i]]
  }
  
  Dic <- list()
  for (i in 1:dim(Meses[[12]])[3]){
    Dic[[i]] <- Meses[[12]][[i]]
  }

  SDAnomalyJan <- lapply(Jan, FUN = function(x) (x - MediasAnuales[[1]])/sdAnuales[[1]])
  SDAnomalyFeb <- lapply(Feb, FUN = function(x) (x - MediasAnuales[[2]])/sdAnuales[[2]])
  SDAnomalyMar <- lapply(Mar, FUN = function(x) (x - MediasAnuales[[3]])/sdAnuales[[3]])
  SDAnomalyAbr <- lapply(Abr, FUN = function(x) (x - MediasAnuales[[4]])/sdAnuales[[4]])
  SDAnomalyMay <- lapply(May, FUN = function(x) (x - MediasAnuales[[5]])/sdAnuales[[5]])
  SDAnomalyJun <- lapply(Jun, FUN = function(x) (x - MediasAnuales[[6]])/sdAnuales[[6]])
  SDAnomalyJul <- lapply(Jul, FUN = function(x) (x - MediasAnuales[[7]])/sdAnuales[[7]])
  SDAnomalyAug <- lapply(Aug, FUN = function(x) (x - MediasAnuales[[8]])/sdAnuales[[8]])
  SDAnomalySep <- lapply(Sep, FUN = function(x) (x - MediasAnuales[[9]])/sdAnuales[[9]])
  SDAnomalyOct <- lapply(Oct, FUN = function(x) (x - MediasAnuales[[10]])/sdAnuales[[10]])
  SDAnomalyNov <- lapply(Nov, FUN = function(x) (x - MediasAnuales[[11]])/sdAnuales[[11]])
  SDAnomalyDic <- lapply(Dic, FUN = function(x) (x - MediasAnuales[[12]])/sdAnuales[[12]])
  
  SDAnomaly <- raster()
  for(i in 1:length(SDAnomalyJan)){
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyJan[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyFeb[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyMar[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyAbr[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyMay[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyJun[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyJul[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyAug[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalySep[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyOct[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyNov[[i]])
  SDAnomaly <- addLayer(SDAnomaly, SDAnomalyDic[[i]])
  }
  
  
  year <- sort(rep(ini:fin, 12))
  names(SDAnomaly) <- paste0("MODIS_", year, "_", 01:12)
  
  return(SDAnomaly)
}
