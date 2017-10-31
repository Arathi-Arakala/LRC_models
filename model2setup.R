
#######################################################################################################
#### This is a set of commands to set up the constants used by the sub-catchment demography model #####
#######################################################################################################

##### read in the initial population based on the zone
inputfile<-"H:/R/KoiHerpes/13072017_Rfiles/initial_population_sizes.xlsx"
data<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
data_zone_index<-which(data$Zone==zone)
data_zone<-as.data.frame(data[data_zone_index,])
init<-c(A_0=data_zone$`Young of Year`, A_1=data_zone$`Yr1`, A_2=data_zone$`Yr2`, A_3=data_zone$`Yr3`, A_4=data_zone$`Adults`)

###### read in spawning suitability data
inputfile<-"H:/R/KoiHerpes/13072017_Rfiles/zone_spawning_events_area.xlsx"
data_spawning<-as.data.frame(read_excel(inputfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))
data_spawning_index<-which(data_spawning$zone==zone)
data_spawning_zone<-as.data.frame(data_spawning[data_spawning_index,])

calendar_year <- data_spawning_zone$year
p_spawning_area <-data_spawning_zone$p_wetland_area
s_spawning_area <-data_spawning_zone$s_wetland_area                                      


###### define w_t (important!!) and associated spawning success
w_t <- p_spawning_area + s_spawning_area + 10          
