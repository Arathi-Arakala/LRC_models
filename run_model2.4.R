######################################################
#### This model build on model 2.3 and will include fish movement at the end of every year
#### We will change order of the loops. 
#### All zones will be run for 1 year, movement will be incorporated, spawning happens
##### Then next year is run.
######################################################
###### top matter
setwd("/Users/Arathi/Documents/2017/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model2.3.R")

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

# M<-matrix(0,nrow=8, ncol=8, byrow=TRUE )
# diag(M)<-1
# P_e<-rep(0, times=8)

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

t_start<-1
for(k in 2:(runtime-1) ){ #(runtime-1)    ##Note we start k at 2 as that corresponds to 1994.

  output_all_zones<-array(0, dim=c(8, 365 ,6))
  output_last<-numeric()
  
  
  for(z in 1:8){
    zone<-z
    if(k==2){
      source("model2.3setup.R")
      init_allZones<-rbind(init_allZones,init)
      wetland_allZones<-rbind(wetland_allZones, BBN_input_demogModel$W_z_t)
      adults_allZones<-rbind(adults_allZones, BBN_input_demogModel$A_z_t)
    }
    
    
    
    ###### Choose the time scale over which you want to run the model
    T <- 1                                             ## w_t repeats T times: T = 1 --> run from January 1994- December 2013 
    wetland <- rep(wetland_allZones[z,], times=T)                       ## repeat the spawning_suit vector to get a much longer time series

    output_all<-numeric() 
    
    #solve the differential equations, update output_all
    init<-init_allZones[z,]
    source("solve2.4.R")
    
    output_all_zones[z, , ]<-as.matrix(output_all)
    output_last<-rbind(output_last, sol_last)
    
    #save all the ouputs per zone
    if(z==1)
      output_all_zone1<-rbind(output_all_zone1, output_all)
    if(z==2)
      output_all_zone2<-rbind(output_all_zone2, output_all)
    if(z==3)
      output_all_zone3<-rbind(output_all_zone3, output_all)
    if(z==4)
      output_all_zone4<-rbind(output_all_zone4, output_all)
    if(z==5)
      output_all_zone5<-rbind(output_all_zone5, output_all)
    if(z==6)
      output_all_zone6<-rbind(output_all_zone6, output_all)
    if(z==7)
      output_all_zone7<-rbind(output_all_zone7, output_all)
    if(z==8)
      output_all_zone8<-rbind(output_all_zone8, output_all)
      
  }# end of z loop
  
  # quartz()
  # par(mfrow=c(2,1), oma=c(0,0,2,0))
  # plot(output_all_zone5$time, output_all_zone5$A_3 + output_all_zone5$A_4, type='l', lwd=2, lty=1, col=1)
  # lines(output_all_zone5$time, output_all_zone5$A_0/100, lwd=2, col=3)
  # 
  # plot(output_all_zone5_model2pt3$time, output_all_zone5_model2pt3$A_3 + output_all_zone5_model2pt3$A_4, type='l', lwd=2, lty=1, col=1)
  # lines(output_all_zone5_model2pt3$time, output_all_zone5_model2pt3$A_0/100, lwd=2, col=3)
  
  #migration, update the A2, A3 and A_4 class using a z loop.
  output_last<-as.matrix(output_last)
  output_migrated<-matrix(0, nrow=dim(output_last)[1], ncol=dim(output_last)[2])
  colnames(output_migrated)<-colnames(output_last)
  
  output_migrated[,1:3]<-output_last[,1:3] #Age class 0 and 1 don't migrate
  #simultaneous update of all zones init
  
  #outflow matrix
  outflow_allZones<-matrix(0, nrow=8, ncol=6)
  #time is first col, A_0, A_1 have no outflow in any zone so those columns are 0
  outflow_allZones[,4]<-output_last[,4]*P_e # outflow of A_2 from all zones
  outflow_allZones[,5]<-output_last[,5]*P_e # outflow of A_3 from all zones
  outflow_allZones[,6]<-output_last[,6]*P_e # outflow of A_4 from all zones
  
  
  #inflow of A_2, A_3, A_4 ageclass into each of the zones
  for(z in 1:8){
    #amount remaining in zone z + amount flowing in from other zones
    #A_2
    output_migrated[z,4]<- (output_last[,4]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,4])[-z])
    #A_3
    output_migrated[z,5]<- (output_last[,5]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,5])[-z])
   #A_4
    output_migrated[z,6]<- (output_last[,6]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,6])[-z])
    
    wetland<-wetland_allZones[z,]
     #age the carp populations in zone z
    A_1<-output_migrated[z,"A_0"]
    A_2<-output_migrated[z,"A_1"]
    A_3<-output_migrated[z,"A_2"]
    A_4<-output_migrated[z,"A_3"] + output_migrated[z,"A_4"] # new adults will be old adults plus incoming adults to this class, as adults live for 30 years.
    
    A_0<-L[1,] %*% as.matrix(c(0 ,A_1, A_2, A_3, A_4)) * spawn_success(wetland[k+1])
    init<-c(A_0=A_0, A_1=A_1, A_2=A_2, A_3=A_3, A_4=A_4)
    #update init
    init_allZones[z,]<-init
  }
  
  

#update time
  t_start<-output_last[,"time"][1]+1
  year <- year+1
  #output_discrete<-rbind(output_discrete, c(year,init))

  }# end of k loop

########################################################################
# visualise all zones

for(z in 1:8){
  output_all<-numeric()
  wetland<-wetland_allZones[z,]
  zone<-z
  if(z==1)
    output_all<-output_all_zone1
  
  if(z==2)
    output_all<-output_all_zone2
  if(z==3)
    output_all<-output_all_zone3
  if(z==4)
    output_all<-output_all_zone4
  if(z==5)
    output_all<-output_all_zone5
  if(z==6)
    output_all<-output_all_zone6
  if(z==7)
    output_all<-output_all_zone7
  if(z==8)
    output_all<-output_all_zone8
  
  #visualise dynamics and behaviour
  source("visualise.R")
  
}#end of z loop

