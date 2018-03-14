
#######################################################################################################
#### This is a set of commands to set up the constants used by the sub-catchment demography model #####
#### read from the new data sent on 20180122 ##########################################################################
#######################################################################################################
z<-zone
#month_range<-1:252


# get data for the zone
rowIndex<-which(data_new$zone==z)
data_zone<-data_new[rowIndex,]
year_range<-seq(from=range(data_zone$iso_year)[1], to=range(data_zone$iso_year)[2], by=1)


#get t_start
first_week<-data_zone$week_no[which(data_zone$iso_year==year_range[1])][1]
t_start<-(first_week*7)-6 # first day of the week.

#remove na
index_na<-which(is.na(data_zone$est_river_ha)==TRUE)
data_zone$est_river_ha[index_na]<-0

index_na<-which(is.na(data_zone$est_wetland_ha)==TRUE)
data_zone$est_wetland_ha[index_na]<-0


# 1. A_z_t
data_A<-data_zone[,2:5]
data_A_yearavg<-rep(0, times=length(year_range))
for(i in 1:length(data_A_yearavg)){
  data_A_yearavg[i]<-mean(data_A$est_adult_no[which(data_A$iso_year==year_range[i])])
}

# make it deterministic for testing
tmp1<-rep(mean(data_A_yearavg), length(data_A_yearavg))

# 2. Read in W_z_t
data_W<-data_zone$est_wetland_ha+(data_zone$est_river_ha*0.05)
W_z_t<-rep(0, times=length(year_range))
for(i in 1:length(W_z_t)){
  ind<-which(data_zone$iso_year==year_range[i])
  tmp<-(data_zone$est_wetland_ha[ind])+(0.05 * data_zone$est_river_ha[ind] )
  tmp[is.na(tmp)==TRUE]<-0
  W_z_t[i]<-mean( tmp )
    
}


# make it deterministic for testing
tmp2<-rep(mean(data_W), times=length(W_z_t))


# 3. Spawning suitability switch, S_z_t
data_S_switch<-data_zone$aggregation_switch
# make it deterministic for testing, turn off aggregation completely
tmp3<-rep(0, times=length(data_S_switch))

# 4. Temparature T_z_t

#data_T<-data_zone$river_avg_water_temp
data_T<-data_zone$waterbody_avg_water_temp

BBN_input_demogModel<-data.frame(year=year_range,A_z_t=data_A_yearavg, W_z_t=W_z_t)
BBN_input_virusModel<-data.frame(year=data_zone$iso_year,week=data_zone$week_no, S_z_t=data_S_switch, T_z_t=data_T)

# ###make it deterministic
# BBN_input_demogModel<-data.frame(year=year_range,A_z_t=tmp1, W_z_t=tmp2)
# BBN_input_virusModel<-data.frame(year=data_zone$iso_year,week=data_zone$week_no, S_z_t=tmp3, T_z_t=data_T)


#init<-c(A_0=data_zone$est_juvenile_no[1], A_1=0, A_2=0, A_3=0, A_4=data_zone$est_adult_no[1])
init<-c(A_0=0, A_1=0, A_2=0, A_3=0, A_4=data_zone$est_adult_no[1])

# ##### read in the initial population based on the zone
# inputfile<-"initial_population_sizes.xlsx"
# data<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
# data_zone_index<-which(data$Zone==zone)
# data_zone_pop<-as.data.frame(data[data_zone_index,])
# init<-c(A_0=data_zone_pop$`Young of Year`, A_1=data_zone_pop$`Yr1`, A_2=data_zone_pop$`Yr2`, A_3=data_zone_pop$`Yr3`, A_4=data_zone_pop$`Adults`)


