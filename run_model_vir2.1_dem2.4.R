####################################################################
### This puts the demography (model2.4) and virus (CyHV3) models together ######
### 14/12/2017 v2 - the model is run monthly. Virus introduced in winter.
####################################################################
setwd("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl);
library(gplots);
library(RColorBrewer)


source("function_library_model_vir2.1_dem2.4_newdata.R")
source("visualise_modelvir2.1_dem2.4.R") #function library having plotting functions

source("initialise_vir2.1_dem2.4.R") #initalise all the global variables

source("weeklyRun_vir2.1_dem2.4_pre.R") # RUN THE MODEL WEEKLY, prior to virus release years

source("weeklyRun_vir2.1_dem2.4_post.R") # RUN THE MODEL WEEKLY, virus is released here






# #############################################################
# #### get output for all zones #####
# results_allzones<-numeric()
# for (z in 1:8){
#   output_all<-numeric()
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
#   zone<-z
#   op<-as.data.frame(getOutputValues(output_all, zone))
#   results_allzones<-rbind(results_allzones, op)
# }
# 
# 
# 
# ########################################################################
#visualise all zones
# 
# for(z in 2:2){
#   output_all<-numeric()
#   wetland<-wetland_allZones[z,]
#   zone<-z
#   if(z==1)
#     output_all<-output_all_zone1
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
#   ### visualise dynamics and behaviour
#   plot_demography(output_all, zone, wetland)
#   plotDiseaseClasses(output_all, zone, wetland)
#   op<-as.data.frame(getOutputValues(output_all, zone))
# }#end of z loop





