####################################################################
### This puts the demography (model2.4) and virus (CyHV3) models together ######
####################################################################
setwd("/Users/Arathi/Documents/2017/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model2.1.R")

year_range<-1993:2013
runtime<-length(year_range)

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

###### set the movement matrix
M=matrix( c(1,0.01,0,0,1,0,0,0,
            1,1,1,0.5,0,1,0,0,
            0,0.45,1,0,0,0,0,0,
            0,0.09,0,1,0,0,0,0,
            0,0,0,0,1,0,0,0,
            0,0.45,0,0,0,1,0,0,
            0,0,0,0,0,0,1,0,
            0,0,0,0.5,0,0,0,1), nrow=8, ncol=8, byrow=TRUE)
P_e<-c(0.01, 0.25, 0.5, 0.25, 0.01, 0.5, 0, 0)

year <- 1994
output_discrete<-numeric()
output_all_zone1<-numeric()
output_all_zone2<-numeric()
output_all_zone3<-numeric()
output_all_zone4<-numeric()
output_all_zone5<-numeric()
output_all_zone6<-numeric()
output_all_zone7<-numeric()
output_all_zone8<-numeric()

init_allZones<-numeric()
wetland_allZones<-numeric()
adults_allZones<-numeric()
spawnSuit_allZones<-numeric()
temp_allZones<-numeric()

t_start<-1
for(k in 2:(runtime-1) ){ #(runtime-1)    ##Note we start k at 2 as that corresponds to 1994.

  output_all_zones<-array(0, dim=c(8, 365 ,6))
  output_last<-numeric()
  
  for(z in 1:8){
    zone<-z
    if(k==2){
      source("model2.1setup.R")
      init_allZones<-rbind(init_allZones,init)
      wetland_allZones<-rbind(wetland_allZones, BBN_input_demogModel$W_z_t)
      adults_allZones<-rbind(adults_allZones, BBN_input_demogModel$A_z_t)
      spawnSuit_allZones<-rbind(spawnSuit_allZones, BBN_input_virusModel$S_z_t)
      temp_allZones<-rbind(temp_allZones, BBN_input_virusModel$T_z_t)
    }
    ###### Choose the time scale over which you want to run the model
    T <- 1                                             ## w_t repeats T times: T = 1 --> run from January 1994- December 2013 
    wetland <- rep(wetland_allZones[z,], times=T)                       ## repeat the spawning_suit vector to get a much longer time series
    output_all<-numeric() 
    
    #solve the differential equations, update output_all
    init<-init_allZones[z,]
    source("solve2.1.R")
    
    
  }#end of zones loop
  }#end of k loop

