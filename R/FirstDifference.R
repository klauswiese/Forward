#Evaluate the first difference of a series
#K. Wiese Julio 2018
##########################################

FirstDifference <- function(r){
  Firstdiff <- calc(r, fun=diff) 
  return(Firstdiff)
}

