
###### top matter
setwd("/Users/Arathi/Documents/2017/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model2.3.R")

###### Choose the zone of the Lachlan Catchment: can be a number from 1-8

for(z in 1:8){
  zone<-z
  
  
  ###### set strength of density dependence
  strength <- 1e-10
  
  ###### set severity of impact of shrinking wetland on survival or age0-2 fish
  q <- 2
  
  ###### set the Leslie matrix
  L=matrix(c(0, 0, 80, 131, 225, 
             0.25, 0, 0, 0, 0, 
             0, 0.5, 0, 0, 0, 
             0, 0, 0.53, 0, 0,
             0, 0, 0, 0.8, 0.8), nrow=5, ncol=5, byrow=TRUE) 
  
  ###### set up the parameters, initial population size and wetland time series
  
  source("model2.3setup.R")
  
  ###### Choose the time scale over which you want to run the model
  T <- 1                                             ## w_t repeats T times: T = 1 --> run from January 1994- December 2013 
  wetland <- rep(BBN_input_demogModel$W_z_t, times=T)                       ## repeat the spawning_suit vector to get a much longer time series
  runtime <- length(wetland)                         ## i.e. T=5 should be 100 years
  
  
  ########### No virus ##########
  ####### Just Demography #######
  
  #start on May 1st 1994: May 1st is the `birthday' of ALL fish not just this first cohort of 6-week fish (Age 0)
  year <- 1994
  output_discrete<-numeric()
  output_all<-numeric()
  
  #solve the differential equations
  source("solve2.3.R")
  
  #visualise dynamics and behaviour
  source("visualise.R")

}
