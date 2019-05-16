#Resilience
#K. Wiese September 2018
########################

Resilience <- function(NANOprev, TANO, SPEI, NANO){
  Input <- stack(NANOprev, TANO, SPEI, NANO)#stack all raster variables 
  tsl <- dim(NANO)[3] #time series length
  #Resilience Beta coeficient, NANO - 1
  funBeta <- function(x) { if (is.na(x[1])){ NA } else lm(x[(3*tsl+1):(4*tsl)] ~ x[1:tsl] + x[(tsl+1):(2*tsl)] + x[(2*tsl+1):(3*tsl)] + 0)$coefficients[1] }
  Beta <- calc(Input, funBeta)
  #Resistence to Temperatura alpha coeficient, TANO  
  funAlpha <- function(x) { if (is.na(x[1])){ NA } else lm(x[(3*tsl+1):(4*tsl)] ~ x[1:tsl] + x[(tsl+1):(2*tsl)] + x[(2*tsl+1):(3*tsl)] + 0)$coefficients[2] }
  Alpha <- calc(Input, funAlpha)
  #Resistence to drought tetha coeficient, SPEI  
  funTetha <- function(x) { if (is.na(x[1])){ NA } else lm(x[(3*tsl+1):(4*tsl)] ~ x[1:tsl] + x[(tsl+1):(2*tsl)] + x[(2*tsl+1):(3*tsl)] + 0)$coefficients[3] }
  Tetha <- calc(Input, funTetha)
  #RMSE
  funRMSE <-  function(x) { if (is.na(x[1])){ NA } else sqrt(mean(lm(x[(3*tsl+1):(4*tsl)] ~ x[1:tsl] + x[(tsl+1):(2*tsl)] + x[(2*tsl+1):(3*tsl)] + 0)$residuals^2)) }
  RMSE <- calc(Input,funRMSE)
  #Results
  Index <- stack(Beta, Alpha, Tetha, RMSE)
  names(Index) <- c("Resilience", "Resistence_to_Temperature", "Resistence_to_Drought", "RMSE")
  return(Index)
}
