
#######################################################################################################
#### This is a set of commands to set up the constants used by the sub-catchment demography model #####
#### same as model 2.3 setup ##########################################################################
#######################################################################################################
z<-zone
year_range<-1993:2013

# 1. Read in A_z_t
inputfile<-paste("BBN_Data/zone_bbn_adult_carp-",z,"_zn.csv", sep="")
data_A<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
# calculate average over the year
data_A_yearavg<-rep(0, times=length(year_range))
for(i in 1:length(data_A_yearavg)){
  start<-i
  
  range_b<-(start-1)*12+1
  range_e<-range_b+11
  
  data_A_yearavg[i]<-mean(data_A$adult_carp_number[range_b:range_e])
}


# 2. Read in W_z_t
RiverArea_hectares<-c(839, 1703, 0, 1312, 492, 0, 85, 410)
inputfile<-paste("BBN_Data/BBN_w_zt.xlsx", sep="")
data_W<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
data_W_zone<-data_W[which(data_W$zone==z),]
W_zone<-(data_W_zone$p_wetland_area/4)+(data_W_zone$s_wetland_area/4)+(0.05*RiverArea_hectares[z])




# 3. Spawning suitability
inputfile<-paste("BBN_Data/zone_adult_aggregation_event-",z,"_zn.csv", sep="")
data_S<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
data_S_switch<-rep(0, times=length(data_S$rn))
data_S_switch[which(data_S$rn==3)]<-1


# 4. Temparature T_z_t
inputfile<-paste("BBN_Data/zone_avg_water_temp-",z,"_zn.csv", sep="")
data_T<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))

BBN_input_demogModel<-data.frame(year=year_range,A_z_t=data_A_yearavg, W_z_t=W_zone)
BBN_input_virusModel<-data.frame(year=year_range, S_z_t=data_S_switch, T_z_t=data_T$avg_est_watertemp)
##### read in the initial population based on the zone
inputfile<-"initial_population_sizes.xlsx"
data<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
data_zone_index<-which(data$Zone==zone)
data_zone<-as.data.frame(data[data_zone_index,])
init<-c(A_0=data_zone$`Young of Year`, A_1=data_zone$`Yr1`, A_2=data_zone$`Yr2`, A_3=data_zone$`Yr3`, A_4=data_zone$`Adults`)


