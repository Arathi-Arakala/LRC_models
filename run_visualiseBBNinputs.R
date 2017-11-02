#################################################################
#### This code will read in all the BBN data and visualise it ###
#### There are four output variables from the BBN ###############
#### 1. A_z_t : Adult pupulation in zone z at time of year t
#### 2. W_z_t : Wetland area in zone z and time of year t
#### 3. S_z_t : spawning suitability in zone z, time t (is a 0/1 value )
#### 4. T_z_t : Temperature of water in zone z, time t.
#################################################################
setwd("/Users/Arathi/Documents/2017/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)

###########################################################################
#### Code to Read in and display BBN inputs from a specific zone z ########
###########################################################################


# Read inputs for all zones - 1 to 8 

for(z in 1:8){
  
  # 1. Read in A_z_t
  inputfile<-paste("BBN_Data/zone_bbn_adult_carp-",z,"_zn.csv", sep="")
  data_A<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
  # calculate average over the year
  data_A_avg<-rep(0, times=length(data_A$ddate))
  for(i in 1:length(data_A$ddate)){
    if(i%%12==0){
      start<-(i/12)
    } 
    if(i%%12>0){
      start<-floor(i/12)+1
    }
    range_b<-(start-1)*12+1
    range_e<-range_b+11
    
    data_A_avg[i]<-mean(data_A$adult_carp_number[range_b:range_e])
  }
  
  time<-1:length(data_A$ddate)
  
  # 2. Read in W_z_t
  inputfile<-paste("BBN_Data/BBN_w_zt.xlsx", sep="")
  data_W<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
  data_W_zone<-data_W[which(data_W$zone==z),]
  W_zone<-data_W_zone$p_wetland_area+data_W_zone$s_wetland_area+10
  
  #creating a monthly w_zone where the year value is duplicated for all months in the year
  w_zone_monthly<-numeric()
  for(i in 1:length(data_W_zone$year)){
    w_zone_monthly<-c(w_zone_monthly,rep(W_zone[i], times=12))
    
  }
  
  time<-1:length(data_A$ddate)
  
  
  # 3. Spawning suitability
  inputfile<-paste("BBN_Data/zone_adult_aggregation_event-",z,"_zn.csv", sep="")
  data_S<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
  data_S_switch<-rep(0, times=length(data_S$rn))
  data_S_switch[which(data_S$rn==3)]<-1
  
  
  # 4. Temparature T_z_t
  inputfile<-paste("BBN_Data/zone_avg_water_temp-",z,"_zn.csv", sep="")
  data_T<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
  
  ### PUT ALL DISPLAYS TOGETHER
  quartz()
  par(mfrow=c(2,2), oma=c(0,0,2,0))
  plot(time/12, data_A$adult_carp_number, type='l', lwd=1, xlab="years", ylab="Adult carp Numbers")
  lines(time/12, data_A_avg, type='l', lwd=2, col=5 )
  
  plot(time/12, w_zone_monthly, type='l', lwd=2, xlab="years", ylab="Wetlands")
  
  plot(time/12,data_S_switch, type='l', lwd=2, col=2, xlab="years", ylab="Aggregation", ylim=c(0,1) )
  
  plot(time/12, data_T$avg_est_watertemp, type='l', col=3, xlab="years", ylab="Temp" )
  abline(h=16, col=3, lwd=2, lty=2)
  abline(h=23, col=3, lwd=2, lty=2)
  
  title(paste("Zone ", z, sep=""), outer=TRUE)
  
}




