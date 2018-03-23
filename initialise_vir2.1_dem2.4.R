###### set strength of density dependence
#strength <- 1e-9 #can play around
strength<-10^-10.3
###### set severity of impact of shrinking wetland on survival or age0-2 fish
q <- 2 #can play around

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

#set the scaling factor for beta matrix
#SF<-1

#year <- 2000
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

#read the master input file
inputfile<-paste("BBN_Data/RMIT_lw_weekly_v2_20180206.csv", sep="")
data_new<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
year_range<-range(data_new$iso_year)[1]:range(data_new$iso_year)[2]

week_range<-1: ( length(data_new$week_no)/range(data_new$zone)[2] ) 
runtime<-length(week_range)

#VirusYear<-2013
#SF<-10^2
# release virus in first week of spring i.e. Aug
#VirusWeek<-32
#VirusZone<-2 # Zone where virus is released
VirusDay<-numeric()#stores the day that the virus is released.

MaxAdults<-0 #max Adults across all zones
MaxAiling<-0 #max ailing across all zones

runtime<-length(week_range)-52 #counting number of weeks to run the loop for. We don't run the last year

#read the habitat zone file
inputfile<-paste("BBN_Data/RMIT_lw_weekly_v4_20180321_zoned_habitats_weekly.csv", sep="")
data_habitat<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
na_index<-which(is.na(data_habitat), arr.ind=TRUE)
data_habitat[na_index]<-0
