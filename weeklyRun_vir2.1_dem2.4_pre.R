##############################################################
### This file runs the model weekly ##########################
## Migration can be turned on and off ########################
MigrationFlag<-0 # 0 is turn off migration, 1 is turn it on
ContRecruitmentFlag<-1 # Flag to indicate continuous recruitment implemented. Do not use the Leslie matrix mthod in this case

### run the model for all weeks until the year 2012, as we will release virus onlyin 2013.
w_stop_index<-(which(data_new$iso_year>2012)[1]-1)/8 # last w for the run.
# for(w in 1:w_stop_index){
#   year <-data_new$iso_year[w]
# 
#   output_last<-numeric() # to save the last day of the year output from model for 8 zones
#   for(z in 1:8){
#     zone<-z
#     if(w==1){
#       source("model_vir2.1_dem2.4_setup_newdata2018.R")
#       init_allZones<-rbind(init_allZones,init)
#       wetland_allZones<-rbind(wetland_allZones, BBN_input_demogModel$W_z_t)
#       adults_allZones<-rbind(adults_allZones, BBN_input_demogModel$A_z_t)
#       spawnSuit_allZones<-rbind(spawnSuit_allZones, BBN_input_virusModel$S_z_t)
#       temp_allZones<-rbind(temp_allZones, BBN_input_virusModel$T_z_t)
#       data_A_allZones<-rbind(data_A_allZones, data_A$est_adult_no)
# 
#     }
#     ###### Choose the time scale over which you want to run the model
#     T <- 1                                             ## w_t repeats T times: T = 1 --> run from January 1994- December 2013
#     wetland <- rep(wetland_allZones[z,], times=T)                       ## repeat the spawning_suit vector to get a much longer time series
#     output_all<-numeric()
# 
#     if(w==1){
#       init_allStates_mx[z,1:5]<-as.numeric(init_allZones[z,])
# 
#     }
#     init_allStates<-init_allStates_mx[z,] #initialise state vector
# 
#     if(zone==VirusZone && BBN_input_virusModel$year[w]==VirusYear && BBN_input_virusModel$week[w]==VirusWeek){
#       init_allStates[14]<-100 # release 100 infected age4 carp.
#       VirusDay<-t_start
#     }
#     #solve the differential equations, update output_all
#     source("solve_vir2.1_dem2.4_newdata.R")
# 
# 
#     output_last<-rbind(output_last, sol_last)
# 
#     #save all the outputs per zone
#     if(z==1)
#       output_all_zone1<-rbind(output_all_zone1, output_all)
#     if(z==2)
#       output_all_zone2<-rbind(output_all_zone2, output_all)
#     if(z==3)
#       output_all_zone3<-rbind(output_all_zone3, output_all)
#     if(z==4)
#       output_all_zone4<-rbind(output_all_zone4, output_all)
#     if(z==5)
#       output_all_zone5<-rbind(output_all_zone5, output_all)
#     if(z==6)
#       output_all_zone6<-rbind(output_all_zone6, output_all)
#     if(z==7)
#       output_all_zone7<-rbind(output_all_zone7, output_all)
#     if(z==8)
#       output_all_zone8<-rbind(output_all_zone8, output_all)
# 
#     A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
#     A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
#     A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`
#     tmp_max<-max(A_2+A_3+A_4)
#     if(tmp_max>MaxAdults) MaxAdults<-tmp_max
# 
# 
#     A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
#     tmp_max<-max(A_adults)
#     if(tmp_max>MaxAiling) MaxAiling<-tmp_max
# 
#   }#end of zones loop
# 
#   ######## if the week_no is 26 (winter of a year), ageing happens
#   ##########################################################
#   if(data_zone$week_no[w]==26){
#     output_last_midyear<-as.matrix(output_last)
#     #Now age the populations
#     for(z in 1:8){
#       wetland<-wetland_allZones[z,]
# 
#       init<-numeric() #create the 30 length initial state for next year
#       for(v in 1:6){
#         index_A0<-((v-1)*5)+2
#         index_A1<-((v-1)*5)+3
#         index_A2<-((v-1)*5)+4
#         index_A3<-((v-1)*5)+5
#         index_A4<-((v-1)*5)+6
# 
#         #age the carp populations in zone z
#         A_1<-output_last_midyear[z,index_A0]
#         A_2<-output_last_midyear[z,index_A1]
#         A_3<-output_last_midyear[z,index_A2]
#         A_4<-output_last_midyear[z,index_A3] + output_last_midyear[z,index_A4] # new adults will be old adults plus incoming adults to this class, as adults live for 30 years.
#         A_0<-0
#         if(ContRecruitmentFlag==0){
#           #recruitment of 6 week old fish from each age class, using leslie matrix method, comment out for cont recruitment
#           A_0<-L[1,] %*% as.matrix(c(0 ,A_1, A_2, A_3, A_4)) * spawn_success(wetland[k+1])
# 
#         }
# 
#         init<-c(init, A_0, A_1, A_2, A_3, A_4)
# 
# 
#       }#end of v loop
#       # update the susceptible age 0 class with age 0 from all other disease classes, only when we use the Leslie matrix approach
#       # and set age 0 class of all other disease states to 0.
#       if(ContRecruitmentFlag==0){
#         init[1]<-init[1]+init[6]+init[11]+init[16]+init[21]+init[26]
#         init[6]<-0
#         init[11]<-0
#         init[16]<-0
#         init[21]<-0
#         init[26]<-0
#       }
# 
# 
#       #update initial states for next year
#       init_allStates_mx[z,]<-as.numeric(init)
#     }#end of z loop
# 
#   } #end of if week_no=26 loop
# 
#   # if it is not mid year
#   if(data_zone$week_no[w]!=26){
#     ######## Migrate at the end of the year
#     # get the 53 week years,
#     # use the data_zone data from last zone as this data is the same for all zones
#     week53_years<-data_zone$iso_year[which(data_zone$week_no==53)]
#     # if we are at the last week of the year i.e. week 52 or week 53 for respective years
#     endOfYearFlag<-(data_zone$iso_year[w]%in% week53_years && data_zone$week_no[w]==53) || ( !(data_zone$iso_year[w]%in% week53_years) && data_zone$week_no[w]==52   )
# 
#     if( endOfYearFlag==TRUE){
#       ######## migration, update the A2, A3 and A4 class using a z loop.
#       output_last<-as.matrix(output_last)
#       output_migrated<-matrix(0, nrow=dim(output_last)[1], ncol=dim(output_last)[2])
#       colnames(output_migrated)<-colnames(output_last)
#       #outflow matrix
#       outflow_allZones<-matrix(0, nrow=dim(output_last)[1], ncol=dim(output_last)[2])
#       colnames(outflow_allZones)<-colnames(output_last)
# 
#       output_migrated[,1]<-output_last[,1] #copy the time column
#       # All infection classes of adults migrate
#       for(v in 1:6){ # loop through the 6 infection classes S,E,I,L,A,Z for each age class.
# 
#         index_A0<-((v-1)*5)+2
#         index_A1<-((v-1)*5)+3
#         index_A2<-((v-1)*5)+4
#         index_A3<-((v-1)*5)+5
#         index_A4<-((v-1)*5)+6
#         output_migrated[,c(index_A0, index_A1)]<-output_last[,c(index_A0, index_A1)] #Age class 0 and 1 don't migrate
# 
#         if(v==5){
#           output_migrated[,c(index_A2, index_A3, index_A4)]<-output_last[,c(index_A2, index_A3, index_A4)] #Force the ailing Age class 2,3 and 4 to not migrate
# 
#         }
# 
#         if(v!=5){
#           if(MigrationFlag==0){
#             # this line added to create no migration across zones
#             output_migrated[,c(index_A2, index_A3, index_A4)]<-output_last[,c(index_A2, index_A3, index_A4)] #Force no migration
# 
#           }
# 
#           if(MigrationFlag==1){
# 
#             #simultaneous update of all zones init
#             #calculate outflow from each age class and virus class.
#             #time is first col, A_0, A_1 have no outflow in any zone so those columns are 0
#             outflow_allZones[,index_A2]<-output_last[,index_A2]*P_e # outflow of A_2 from all zones
#             outflow_allZones[,index_A3]<-output_last[,index_A3]*P_e # outflow of A_3 from all zones
#             outflow_allZones[,index_A4]<-output_last[,index_A4]*P_e # outflow of A_4 from all zones
# 
#             ############
#             #inflow of A_2, A_3, A_4 ageclass into each of the zones
#             for(z in 1:8){
#               #amount remaining in zone z + amount flowing in from other zones
#               #A_2
#               output_migrated[z,index_A2]<- (output_last[,index_A2]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A2])[-z])
#               #A_3
#               output_migrated[z,index_A3]<- (output_last[,index_A3]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A3])[-z])
#               #A_4
#               output_migrated[z,index_A4]<- (output_last[,index_A4]*(1-P_e))[z] + sum( (M[z,]*outflow_allZones[,index_A4])[-z])
#             }#end of z loop
#           }
# 
# 
#         }#end of if loop
# 
# 
# 
#       }#end of v loop
# 
# 
# 
#       init_allStates_mx<-output_migrated[,-1]
# 
#     }#end of if loop
# 
#     #if it is not end of the year, initialise the inital state vectors.
#     if(endOfYearFlag==FALSE){
#       output_last<-as.matrix(output_last)
#       init_allStates_mx<-output_last[,-1]
#     }
# 
#   }
# 
# 
#   #update time
#   t_start<-as.numeric(output_last[,"time"][1]+1)
#   #output_discrete<-rbind(output_discrete, c(year,init))
#   print(c(data_zone$iso_year[w], data_zone$week_no[w]))
# }# end of w loop

# save the data files
outputfile<-"/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models/OutputData/preVirusReleaseData.Rdata"
#save(init_allStates_mx, t_start, output_all_zone1, output_all_zone2, output_all_zone3, output_all_zone4, output_all_zone5, output_all_zone6, output_all_zone7, output_all_zone8,init_allZones,wetland_allZones,adults_allZones,spawnSuit_allZones, temp_allZones,data_A_allZones , file=outputfile)
load(outputfile)
t_start_new<-t_start