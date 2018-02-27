# This code will loop the run_..model to diff parameter ranges

setwd("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model_vir2.1_dem2.3_newdata.R")

#scaling factor
S_range<-10^seq(from=1, to=4, by=1)
#S_range<-1
#S_range<-seq(from=100, to=1000, by=100)
#virus years
#VirusYear_range<-c(5,7,9)
VirusYear_range<-c(5)

#sensitivity to beta scaling factor
for(v in 1:length(VirusYear_range)){
  VirusYear<-1999+VirusYear_range[v]
  for(S in 1:length(S_range)){
    SF<-S_range[S] #scaling factor for beta
    source("run_model_vir2.1_dem2.3.R")
  }
}


# #sensitivity to density dependence
# Strength_range<-10^seq(from=-10.2, to=-9.5, by=0.05)
# for(v in 1:length(VirusYear_range)){
#   VirusYear<-1999+VirusYear_range[v]
#   for(S in 1:length(Strength_range)){
#     strength<-Strength_range[S] #scaling factor for beta
#     source("run_model_vir2.1_dem2.3.R")
#   }
# }