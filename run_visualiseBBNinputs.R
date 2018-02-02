#################################################################
#### This code will read in all the BBN data and visualise it ###
#### There are four output variables from the BBN ###############
#### 1. A_z_t : Adult pupulation in zone z at time of year t
#### 2. W_z_t : Wetland area in zone z and time of year t
#### 3. S_z_t : spawning suitability in zone z, time t (is a 0/1 value )
#### 4. T_z_t : Temperature of water in zone z, time t.
#################################################################
setwd("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)

###########################################################################
#### Code to Read in and display BBN inputs from a specific zone z ########
###########################################################################


# # Read inputs for all zones - 1 to 8 
# 
# for(z in 1:8){
#   
#   # 1. Read in A_z_t
#   inputfile<-paste("BBN_Data/zone_bbn_adult_carp-",z,"_zn.csv", sep="")
#   data_A<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
#   # calculate average over the year
#   data_A_avg<-rep(0, times=length(data_A$ddate))
#   for(i in 1:length(data_A$ddate)){
#     if(i%%12==0){
#       start<-(i/12)
#     } 
#     if(i%%12>0){
#       start<-floor(i/12)+1
#     }
#     range_b<-(start-1)*12+1
#     range_e<-range_b+11
#     
#     data_A_avg[i]<-mean(data_A$adult_carp_number[range_b:range_e])
#   }
#   
#   time<-1:length(data_A$ddate)
#   
#   # 2. Read in W_z_t
#   RiverArea_hectares<-c(839, 1703, 0, 1312, 492, 0, 85, 410)
#   inputfile<-paste("BBN_Data/BBN_w_zt.xlsx", sep="")
#   data_W<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
#   data_W_zone<-data_W[which(data_W$zone==z),]
#   W_zone<-(data_W_zone$p_wetland_area/4)+(data_W_zone$s_wetland_area/4)+(0.05*RiverArea_hectares[z])
#   
#   #creating a monthly w_zone where the year value is duplicated for all months in the year
#   w_zone_monthly<-numeric()
#   for(i in 1:length(data_W_zone$year)){
#     w_zone_monthly<-c(w_zone_monthly,rep(W_zone[i], times=12))
#     
#   }
#   
#   time<-1:length(data_A$ddate)
#   
#   
#   # 3. Spawning suitability
#   inputfile<-paste("BBN_Data/zone_adult_aggregation_event-",z,"_zn.csv", sep="")
#   data_S<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
#   data_S_switch<-rep(0, times=length(data_S$rn))
#   data_S_switch[which(data_S$rn==3)]<-1
#   
#   
#   # 4. Temparature T_z_t
#   inputfile<-paste("BBN_Data/zone_avg_water_temp-",z,"_zn.csv", sep="")
#   data_T<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
#   
#   ### PUT ALL DISPLAYS TOGETHER
#   quartz()
#   par(mfrow=c(2,2), oma=c(0,0,2,0))
#   plot(time/12, data_A$adult_carp_number, type='l', lwd=1, xlab="years", ylab="Adult carp Numbers")
#   lines(time/12, data_A_avg, type='l', lwd=2, col=5 )
#   
#   plot(time/12, w_zone_monthly, type='l', lwd=2, xlab="years", ylab="Wetlands")
#   
#   plot(time/12,data_S_switch, type='l', lwd=2, col=2, xlab="years", ylab="Aggregation", ylim=c(0,1) )
#   
#   plot(time/12, data_T$avg_est_watertemp, type='l', col=3, xlab="years", ylab="Temp" )
#   abline(h=16, col=3, lwd=2, lty=2)
#   abline(h=23, col=3, lwd=2, lty=2)
#   
#   title(paste("Zone ", z, sep=""), outer=TRUE)
#   
# }


################### READ IN new Data sent by Kerryne ############

inputfile<-paste("BBN_Data/RMIT_lw_weekly_v1_20180122.csv", sep="")
data_new<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))

# separate the zone data
data_zone1<-numeric()
data_zone2<-numeric()
data_zone3<-numeric()
data_zone4<-numeric()
data_zone5<-numeric()
data_zone6<-numeric()
data_zone7<-numeric()
data_zone8<-numeric()

ymax<-rep(0, times=5)
for(i in 1:5)
{
  if(i==1)
    ymax[i]<-max(data_new$est_adult_no)
  if(i==2){
    index_na<-which(is.na(data_new$est_river_ha)==TRUE)
    data_new$est_river_ha[index_na]<-0
    index_na<-which(is.na(data_new$est_wetland_ha)==TRUE)
    data_new$est_wetland_ha[index_na]<-0
    
    ymax[i]<-max(data_new$est_wetland_ha+(0.05*data_new$est_river_ha) ) 
    
  }
  if(i==3)
    ymax[i]<-max(data_new$aggregation_switch)
  if(i==4)
    ymax[i]<-max(data_new$waterbody_avg_water_temp)
  if(i==5)
    ymax[i]<-max(data_new$river_avg_water_temp)
  
}

for(z in 1:8){
  rowIndex<-which(data_new$zone==z)
  if(z==1)
    data_zone1<-data_new[rowIndex,]
  if(z==2)
    data_zone2<-data_new[rowIndex,]
  if(z==3)
    data_zone3<-data_new[rowIndex,]
  if(z==4)
    data_zone4<-data_new[rowIndex,]
  if(z==5)
    data_zone5<-data_new[rowIndex,]
  if(z==6)
    data_zone6<-data_new[rowIndex,]
  if(z==7)
    data_zone7<-data_new[rowIndex,]
  if(z==8)
    data_zone8<-data_new[rowIndex,]
}
data_toView<-numeric()

for(i in 1:5){
    
  quartz()
  par(mfrow=c(3,3), oma=c(0,0,2,0))
  for(z in 1:8){
    if(z==1)
      data_toView<-data_zone1
    if(z==2)
      data_toView<-data_zone2
    if(z==3)
      data_toView<-data_zone3
    if(z==4)
      data_toView<-data_zone4
    if(z==5)
      data_toView<-data_zone5
    if(z==6)
      data_toView<-data_zone6
    if(z==7)
      data_toView<-data_zone7
    if(z==8)
      data_toView<-data_zone8
    
    dataToDisplay<-""
    dispValue<-numeric()
    
       y_max<-ymax[i]
    if(i==1) {
      dataToDisplay<-"Adults"
      dispValue<-data_toView$est_adult_no
      
      }
    if(i==2){
      dataToDisplay<-"Wetland + 5% River Area (ha)"
      dispValue<-data_toView$est_wetland_ha+(data_toView$est_river_ha*0.05)
      dispValue[which(is.na(dispValue)==TRUE)]<-0
      
    } 
    if(i==3){
      dataToDisplay<-"Aggregation"
      dispValue<-data_toView$aggregation_switch
    } 
    if(i==4){
      dataToDisplay<-"Waterbody Temperature"
      dispValue<-data_toView$waterbody_avg_water_temp

    } 
   if(i==5){
     dataToDisplay<-"River Temperature"
     dispValue<-data_toView$river_avg_water_temp
     
   } 
     
    if(z<=4 || z>=6){
      newcol<-data_toView$iso_year+(data_toView$week_no/52)
      plot(newcol, dispValue, type='l', lty=1, lwd=2, ylim=c(0, y_max), main=paste("zone", z, sep=" "), ylab=dataToDisplay, col=i)
    }
    if(z==5){
      plot(0, type="n", axes=F, xlab="", ylab="", ylim=c(0,5), xlim=c(0,5))
      text(x=2.5, y=2.5, dataToDisplay, cex=1.5, pos=3)
      
      newcol<-data_toView$iso_year+(data_toView$week_no/52)
      plot(newcol, dispValue, type='l', lty=1, lwd=2, ylim=c(0, y_max), main=paste("zone", z, sep=" "), ylab=dataToDisplay, col=i )
      
    }
    
    
  }
  
  
}

