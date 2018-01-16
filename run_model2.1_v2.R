####################################################################
### This puts the demography (model2.4) and virus (CyHV3) models together ######
### 14/12/2017 v2 - the model is run monthly. Virus introduced in winter.
####################################################################
setwd("/Users/Arathi/Documents/2017/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model2.1.R")

year_range<-1993:2013
#runtime<-length(year_range)
month_range<-1:(length(year_range)*12)

runtime<-length(month_range)
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
data_A_allZones<-numeric()
init_allStates_mx<-matrix(0, nrow=8, ncol=30)

VirusYear<-1
VirusMonth<-((VirusYear-1)*12) + 6 # winter in VirusYear.
VirusZone<-2 # Zone where virus is released
VirusDay<-numeric()#stores the day that the virus is released.
DaysInAMonth<-c(31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31)

MaxAdults<-0 #max Adults across all zones
MaxAiling<-0 #max ailing across all zones

t_start<-366 # end of year 1993
#for(m in 13:36 ){ #(runtime-1)    ##Note we start k at 2 as that corresponds to 1994.
for(m in 13: (runtime-13) ){ # we start in the year 1994.End in Dec of the last but one year.
  month_index<-m%%12
  if(m%%12==0) month_index<-12
  output_last<-numeric() # to save the last day of the year output from model for 8 zones
  
  for(z in 1:8){
    zone<-z
    if(m==13){
      source("model2.1setup.R")
      init_allZones<-rbind(init_allZones,init)
      wetland_allZones<-rbind(wetland_allZones, BBN_input_demogModel$W_z_t)
      adults_allZones<-rbind(adults_allZones, BBN_input_demogModel$A_z_t)
      spawnSuit_allZones<-rbind(spawnSuit_allZones, BBN_input_virusModel$S_z_t)
      temp_allZones<-rbind(temp_allZones, BBN_input_virusModel$T_z_t)
      data_A_allZones<-rbind(data_A_allZones, data_A$adult_carp_number)
    }
    ###### Choose the time scale over which you want to run the model
    T <- 1                                             ## w_t repeats T times: T = 1 --> run from January 1994- December 2013 
    wetland <- rep(wetland_allZones[z,], times=T)                       ## repeat the spawning_suit vector to get a much longer time series
    output_all<-numeric() 
    
    if(m==13){
      init_allStates_mx[z,1:5]<-as.numeric(init_allZones[z,])
      
    }
    init_allStates<-init_allStates_mx[z,] #initialise state vector
    
    if(m==VirusMonth && z==VirusZone){
         init_allStates[14]<-100 # release 100 infected age4 carp.
         VirusDay<-t_start
    }
    #solve the differential equations, update output_all
    source("solve2.1_v2.R")
    

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
    
      A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
      A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
      A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`
      tmp_max<-max(A_2+A_3+A_4)
      if(tmp_max>MaxAdults) MaxAdults<-tmp_max
      
    
      A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
      tmp_max<-max(A_adults)
      if(tmp_max>MaxAiling) MaxAiling<-tmp_max
      
  }#end of zones loop
  
  
  ######## Migrate First, Then Age, only at the end of the year
  
  if(m%%12==0){
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
    
    year <- year+1
    
    
  }#end of if loop
  
  #if it is middle of the year, initialise the inital state vectors.
  if(m%%12!=0){
    output_last<-as.matrix(output_last)
    init_allStates_mx<-output_last[,-1]
    
  }
    
  #update time
  t_start<-as.numeric(output_last[,"time"][1]+1)
  #output_discrete<-rbind(output_discrete, c(year,init))
  print(m)
  }#end of m loop

########################################################################
# visualise all zones

# for(z in 1:8){
#   output_all<-numeric()
#   wetland<-wetland_allZones[z,]
#   zone<-z
#   if(z==1)
#     output_all<-output_all_zone1
#   
#   if(z==2)
#     output_all<-output_all_zone2
#   if(z==3)
#     output_all<-output_all_zone3
#   if(z==4)
#     output_all<-output_all_zone4
#   if(z==5)
#     output_all<-output_all_zone5
#   if(z==6)
#     output_all<-output_all_zone6
#   if(z==7)
#     output_all<-output_all_zone7
#   if(z==8)
#     output_all<-output_all_zone8
#   
#   # #visualise dynamics and behaviour
#   source("visualise_model2.1_v2.R")
#   
# 
#   
#   
#   
# }#end of z loop

# PLOT 1 : ############################ create plots for presentation
#compute ylimit


adults_ailing_flag<-0 # if flag=1, display ailing carp. If flag=0 display total adult carp.
quartz()
par(mfrow=c(3,3), oma=c(0,0,2,0))

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
    
    A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
    A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
    A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`
    
    
    A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
    virus_start<-(((VirusYear-1)*365)+1)
    range_days<-virus_start:(virus_start+365)
    A_max<-max(A_adults) # max number of ailing fish in a day.
    maxDay<-output_all$time[which(A_adults==A_max)][1] #Day when maximum fish die.
    maxYear<-floor(maxDay/365) #year when max fish are ailing.
    range_days_2<-(((maxYear-1)*365)+1):(maxYear*365)
    out_of_range<-which(range_days_2>length(A_adults))
    if(length(out_of_range>0)) 
      range_days_2<-range_days_2[-out_of_range]

    totalDeath<-sum(A_adults[range_days_2])
    endOfPrimaryInfection<-VirusDay+(which(A_adults[maxDay:(virus_start+365) ]<1)[1])
    
    if(adults_ailing_flag==0){
      ylimit<-MaxAdults
      
      if(z==1 || z==2 || z==3 || z==4){
        plot(1994.333+output_all$time/365, A_2+A_3+A_4, type="l", lwd=2, col=1, ylab="Adult Carp Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
        abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
        text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      }
      if(z==5){
        plot(0, type="n", axes=F, xlab="", ylab="", ylim=c(0,5), xlim=c(0,5))
        text(x=2.5, y=2.5, "Case 2 - no virus release", cex=1.5, pos=3)
      }
      if(z==5 || z==6 || z==7 || z==8){
        plot(1994.333+output_all$time/365, A_2+A_3+A_4, type="l", lwd=2, col=1, ylab="Adult Carp Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
        abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
        text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      }
      
      
    }
    if(adults_ailing_flag==1){
      ylimit<-MaxAiling
      if(z==1 || z==2 || z==3 || z==4){
        plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=1, ylab="Ailing Adult Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
        abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
        text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
        text(x=2010, y=3*ylimit/4, paste("Ailing_maxYear  =", round(totalDeath), sep=""), pos=1)
      }
      if(z==5){
        plot(0, type="n", axes=F, xlab="", ylab="", ylim=c(0,5), xlim=c(0,5))
        text(x=2.5, y=2.5, "Case 2 - 2003, zone 2, with aggregation", cex=1.5, pos=3)
      }
      if(z==5 || z==6 || z==7 || z==8){
        plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=1, ylab="Ailing Adult Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
        abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
        text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
        text(x=2010, y=3*ylimit/4, paste("Ailing_maxYear =", round(totalDeath), sep=""), pos=1)
        
      }
      
      
    }
    
}

# PLOT 2: ##################################
### plot to compare with bbn result ###########
# create the equivalent x-axis values for bbn output
tmp<-1
for(t in 2:12){
  tmp<-c(tmp, (tmp[length(tmp)]+DaysInAMonth[t-1]) )
}
yr<-rep(BBN_input_demogModel$year, times=1, each=12)
tmp_mth<-rep(tmp, times=21)
tmp_mth<-tmp_mth/365
yr_BBN<-yr+tmp_mth



adults_ailing_flag<-0 # if flag=1, display ailing carp. If flag=0 display total adult carp.
quartz()
par(mfrow=c(3,3), oma=c(0,0,2,0))

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
  
  A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
  A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
  A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`
  
  
  A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
  virus_start<-(((VirusYear-1)*365)+1)
  range_days<-virus_start:(virus_start+365)
  A_max<-max(A_adults) # max number of ailing fish in a day.
  maxDay<-output_all$time[which(A_adults==A_max)][1] #Day when maximum fish die.
  maxYear<-floor(maxDay/365) #year when max fish are ailing.
  range_days_2<-(((maxYear-1)*365)+1):(maxYear*365)
  out_of_range<-which(range_days_2>length(A_adults))
  if(length(out_of_range>0)) 
    range_days_2<-range_days_2[-out_of_range]
  
  totalDeath<-sum(A_adults[range_days_2])
  endOfPrimaryInfection<-VirusDay+(which(A_adults[maxDay:(virus_start+365) ]<1)[1])
  
  if(adults_ailing_flag==0){
    ylimit<-MaxAdults
    
    if(z==1 || z==2 || z==3 || z==4){
      plot(1994.333+output_all$time/365, A_2+A_3+A_4, type="l", lwd=2, col=1, ylab="Adult Carp Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
      abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
      text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      lines(yr_BBN[-(1:12)], data_A_allZones[z,-(1:12)], lty=1, lwd=2, col="blue")
    }
    if(z==5){
      plot(0, type="n", axes=F, xlab="", ylab="", ylim=c(0,5), xlim=c(0,5))
      text(x=2.5, y=2.5, "Case 2 - no virus release", cex=1.5, pos=3)
    }
    if(z==5 || z==6 || z==7 || z==8){
      plot(1994.333+output_all$time/365, A_2+A_3+A_4, type="l", lwd=2, col=1, ylab="Adult Carp Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
      abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
      text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      lines(yr_BBN[-(1:12)], data_A_allZones[z,-(1:12)], lty=1, lwd=2, col="blue")
      
    }
    
    
  }
  if(adults_ailing_flag==1){
    ylimit<-MaxAiling
    if(z==1 || z==2 || z==3 || z==4){
      plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=1, ylab="Ailing Adult Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
      abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
      text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      text(x=2010, y=3*ylimit/4, paste("Ailing_maxYear  =", round(totalDeath), sep=""), pos=1)
    }
    if(z==5){
      plot(0, type="n", axes=F, xlab="", ylab="", ylim=c(0,5), xlim=c(0,5))
      text(x=2.5, y=2.5, "Case 2 - 2003, zone 2, with aggregation", cex=1.5, pos=3)
    }
    if(z==5 || z==6 || z==7 || z==8){
      plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=1, ylab="Ailing Adult Numbers", xlab="year", ylim=c(0,ylimit), main=paste("zone", z, sep=" "))
      abline(v=1994.333+VirusDay/365, col=2, lty=2, lwd=2)
      text(x=1994.333+VirusDay/365, y=0, "Virus Released", pos=2, cex=1)
      text(x=2010, y=3*ylimit/4, paste("Ailing_maxYear =", round(totalDeath), sep=""), pos=1)
      
    }
    
    
  }
  
}

