#MODIS monthly Time Series derived from 8 days times series
###########################################################

monthlyMODIS <- function(MODISyear, year){
  
  Jan <- calc(MODISyear[[1:4]], fun=mean)
  Feb <- calc(MODISyear[[5:8]], fun=mean)
  Mar <- calc(MODISyear[[9:12]], fun=mean)
  Abr <- calc(MODISyear[[13:15]], fun=mean)
  May <- calc(MODISyear[[16:19]], fun=mean)
  Jun <- calc(MODISyear[[20:23]], fun=mean)
  Jul <- calc(MODISyear[[24:27]], fun=mean)
  Aug <- calc(MODISyear[[28:31]], fun=mean)
  Sep <- calc(MODISyear[[32:35]], fun=mean)
  Oct <- calc(MODISyear[[36:38]], fun=mean)
  Nov <- calc(MODISyear[[39:42]], fun=mean)
  Dic <- calc(MODISyear[[43:46]], fun=mean)
  
  monthlymean <- stack(Jan, Feb, Mar, Abr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dic)
  names(monthlymean) <- paste0("MODIS_", year, "_", 01:12)
  return(monthlymean)
}
