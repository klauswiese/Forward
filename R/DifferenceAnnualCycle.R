#Ratio Annual Cycle ACF
#K. Wiese Julio 2018
#######################

DifferenceAnnualCycle <- function(ACF, ASL){
  difference <- function(x,y) x-y
  DiffACFAnnualCycle <- overlay(ACF[[1]], ACF[[ASL]], fun=difference)
  return(DiffACFAnnualCycle)
}