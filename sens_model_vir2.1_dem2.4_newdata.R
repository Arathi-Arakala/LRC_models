# This code will loop the run_..model to diff parameter ranges

setwd("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models")
library(deSolve); library(readxl)
source("function_library_model_vir2.1_dem2.4_newdata.R")

#scaling factor
#S_range<-10^seq(from=1, to=4, by=1)
#S_range<-1
#S_range<-seq(from=100, to=1000, by=100)
#virus years
#VirusYear_range<-c(5,7,9)
#S_range<-c(1000, 100, 10, 1, 0.1)
VirusYear_range<-c(14)
VirusWeek_range<-seq(from=32, to=52, by=1)
S_range<-10^seq(from=3, to=1, by=-0.2)


op_maxDead<-matrix(0, nrow=length(S_range), ncol=length(VirusWeek_range))
op_totalDead<-matrix(0, nrow=length(S_range), ncol=length(VirusWeek_range))
op_delayToPeak<-matrix(0, nrow=length(S_range), ncol=length(VirusWeek_range))

rownames(op_maxDead)<-as.character(S_range)
rownames(op_totalDead)<-as.character(S_range)
rownames(op_delayToPeak)<-as.character(S_range)

colnames(op_maxDead)<-as.character(VirusWeek_range)
colnames(op_totalDead)<-as.character(VirusWeek_range)
colnames(op_delayToPeak)<-as.character(VirusWeek_range)

#sensitivity to beta scaling factor
for(v_y in 1:length(VirusYear_range)){
  VirusYear<-1999+VirusYear_range[v_y]
  for(u in 1:length(VirusWeek_range)){
    VirusWeek<-VirusWeek_range[u]
    for(S in 1:length(S_range)){
      print(c(S, u, v_y))
      
      SF<-S_range[S] #scaling factor for beta
      source("run_model_vir2.1_dem2.4.R")
      ############################################################
      ### get output for zone 2 only #############################
      output_all<-output_all_zone2
      zone<-2
      op<-as.data.frame(getOutputValues(output_all, zone))
      op_maxDead[S,u]<-op$maxDead
      op_totalDead[S,u]<-op$totalDead
      op_delayToPeak[S,u]<-op$delayToPeak
      print(paste("Year =", VirusYear, "Week = ", VirusWeek, "SF =", SF, sep=" "))
    }
  }
}

 outputfile_dest<-"/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models/OutputData/"
 outputfile<-paste(outputfile_dest,"OutputMatrices_",VirusYear,"_zone2_NoTx.Rdata", sep=" ")
 save(op_maxDead, op_totalDead, op_delayToPeak, file=outputfile)
 load(outputfile)

plotHeatMap(op_maxDead/5, "maximum Death")
plotHeatMap(op_totalDead/5, "total Death")
plotHeatMap(op_delayToPeak, "Days to Max Death")

# #sensitivity to density dependence
# Strength_range<-10^seq(from=-10.2, to=-9.5, by=0.05)
# for(v in 1:length(VirusYear_range)){
#   VirusYear<-1999+VirusYear_range[v]
#   for(S in 1:length(Strength_range)){
#     strength<-Strength_range[S] #scaling factor for beta
#     source("run_model_vir2.1_dem2.3.R")
#   }
# }