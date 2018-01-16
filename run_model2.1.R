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
init_allStates_mx<-matrix(0, nrow=8, ncol=30)

VirusYear<-10
VirusZone<-2 # Zone where virus is released
VirusDay<-numeric()#stores the day that the virus is released.

t_start<-1
for(k in 2:(runtime-1) ){ #(runtime-1)    ##Note we start k at 2 as that corresponds to 1994.
#for(k in 2:5 ){ 
  output_last<-numeric() # to save the last day of the year output from model for 8 zones
  
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
    
    if(k==2){
      init_allStates_mx[z,1:5]<-as.numeric(init_allZones[z,])
      
    }
    init_allStates<-init_allStates_mx[z,] #initialise state vector
    
    if(k==VirusYear && z==VirusZone){
         init_allStates[14]<-100 # release 100 infected age4 carp.
         VirusDay<-t_start
    }
    #solve the differential equations, update output_all
    source("solve2.1.R")
    
    #output_all_zones[z, , ]<-as.matrix(output_all)
    output_last<-rbind(output_last, sol_last)
    #save all the outputs per zone
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
    
    
    
  }#end of zones loop
  
  
  ######## Migrate First, Then Age
  ######## migration, update the A2, A3 and A4 class using a z loop.
  output_last<-as.matrix(output_last)
  output_migrated<-matrix(0, nrow=dim(output_last)[1], ncol=dim(output_last)[2])
  colnames(output_migrated)<-colnames(output_last)
  #outflow matrix
  outflow_allZones<-matrix(0, nrow=dim(output_last)[1], ncol=dim(output_last)[2])
  colnames(outflow_allZones)<-colnames(output_last)
  
  output_migrated[,1]<-output_last[,1] #copy the time column
  # All infection classes of adults migrate 
  for(v in 1:6){ # loop through the 6 infection classes S,E,I,L,A,Z for each age class.
    
  index_A0<-((v-1)*5)+2
  index_A1<-((v-1)*5)+3
  index_A2<-((v-1)*5)+4
  index_A3<-((v-1)*5)+5
  index_A4<-((v-1)*5)+6
  output_migrated[,c(index_A0, index_A1)]<-output_last[,c(index_A0, index_A1)] #Age class 0 and 1 don't migrate
 
  if(v==5){
    output_migrated[,c(index_A2, index_A3, index_A4)]<-output_last[,c(index_A2, index_A3, index_A4)] #Age class 0 and 1 don't migrate
    
  }
  
  if(v!=5){
    #simultaneous update of all zones init
    #calculate outflow from each age class and virus class.
    #time is first col, A_0, A_1 have no outflow in any zone so those columns are 0
    outflow_allZones[,index_A2]<-output_last[,index_A2]*P_e # outflow of A_2 from all zones
    outflow_allZones[,index_A3]<-output_last[,index_A3]*P_e # outflow of A_3 from all zones
    outflow_allZones[,index_A4]<-output_last[,index_A4]*P_e # outflow of A_4 from all zones
    
    ############
    #inflow of A_2, A_3, A_4 ageclass into each of the zones
    for(z in 1:8){
      #amount remaining in zone z + amount flowing in from other zones
      #A_2
      output_migrated[z,index_A2]<- (output_last[,index_A2]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A2])[-z])
      #A_3
      output_migrated[z,index_A3]<- (output_last[,index_A3]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A3])[-z])
      #A_4
      output_migrated[z,index_A4]<- (output_last[,index_A4]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A4])[-z])
    }#end of z loop
    
  }
  
 
  }#end of v loop
    
    
#Now age the populations
 for(z in 1:8){
      wetland<-wetland_allZones[z,]
      
      init<-numeric() #create the 30 length initial state for next year
      for(v in 1:6){
        index_A0<-((v-1)*5)+2
        index_A1<-((v-1)*5)+3
        index_A2<-((v-1)*5)+4
        index_A3<-((v-1)*5)+5
        index_A4<-((v-1)*5)+6
      
      #age the carp populations in zone z
      A_1<-output_migrated[z,index_A0]
      A_2<-output_migrated[z,index_A1]
      A_3<-output_migrated[z,index_A2]
      A_4<-output_migrated[z,index_A3] + output_migrated[z,index_A4] # new adults will be old adults plus incoming adults to this class, as adults live for 30 years.
      
      A_0<-L[1,] %*% as.matrix(c(0 ,A_1, A_2, A_3, A_4)) * spawn_success(wetland[k+1])
      
      init<-c(init, A_0, A_1, A_2, A_3, A_4)

      
    }#end of v loop
      #update initial states for next year
      init_allStates_mx[z,]<-as.numeric(init)
  }#end of z loop
 
  #update time
  t_start<-as.numeric(output_last[,"time"][1]+1)
  year <- year+1
  #output_discrete<-rbind(output_discrete, c(year,init))
  print(k)
  }#end of k loop

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
  source("visualise_model2.1.R")
  
  
}#end of z loop

