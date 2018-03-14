##### Weekly run #########

  dt <- 1
  days<-7
  times <- seq(t_start,(t_start+days-1),dt)
  if(w%%52==0) k<-w/52
  if(w%%52>0) k<-floor(w/52)+1 # the year index
  
  
  # calculate wetland change scaling factor for time period k
  if (wetland[k]==0){
    wetland_change <- 1
  }
  if(wetland[k]>0)  {
    wetland_change <- wetland[k+1]/wetland[k]
    if (wetland_change>1) wetland_change <- 1
    if(wetland_change==0) wetland_change<- 1e-12
    wetland_change <- (wetland_change)^(1/q)
  }

  #calculate wetland area value for the current week
  W_zt<-data_W[w]
  
  # calculate sensitivity, the scaling factor for strength of density dependence
  # use a smoothed version of the weekly BBN data, use smoothing window size = 26 weeks (approx 6 months)
  smoothed_data<-smoothBBNdata(data_A_allZones[z,], 26, 0)
    sensitivity<-sum(init[-1])/(smoothed_data[w]+1)
  
  parameters_demog<-set_parameters_model2pt4(wetland_change, strength, L, sensitivity, W_zt)
  parameters<-set_parameters_model_vir2pt1_dem2pt4_newdata(parameters_demog, parameters_virus, zone, w, SF)
  
  # dt <- 1
 # t <- seq(t_start,365*(k-1),dt)
 #times<-seq(t_start, 5, dt)
  
  init<-init_allZones[z,]
  # set up the init vector
  # first 5 are S of the 5 age classes, next 5 are E, 
  # next 5 are I, next 5 are L, next 5 are A, next 5 are Z
  ###############
  
  sol <- as.data.frame(ode(init_allStates, times, CyHV3_5AgeClasses_vir2pt1_dem2pt4, parameters, method="lsoda"))
  output_all<-rbind(output_all, sol)
  sol_last<-output_all[dim(output_all)[1],]
  #sol[dim(sol)[1],]
 
