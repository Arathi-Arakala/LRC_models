# This code will loop the run_..model to diff parameter ranges

setwd("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model_vir2.1_dem2.3.R")

#scaling factor
S_range<-10^seq(from=3, to=4, by=0.2)
#S_range<-0
#virus years
#VirusYear_range<-c(5,7,9)
VirusYear_range<-c(5)

for(v in 1:length(VirusYear_range)){
  VirusYear<-VirusYear_range[v]
  for(S in 1:length(S_range)){
    SF<-S_range[S] #scaling factor for beta
    source("run_model_vir2.1_dem2.3.R")
  }
}
