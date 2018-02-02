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

  
  # calculate sensitivity, the scaling factor for strength of density dependence
    sensitivity<-sum(init[-1])/(adults_allZones[z,k]+1)
  
  parameters_demog<-set_parameters_model2pt3(wetland_change, strength, L, sensitivity)
  parameters<-set_parameters_model_vir2pt1_dem2pt3_newdata(parameters_demog, parameters_virus, zone, w, SF)
  
  # dt <- 1
 # t <- seq(t_start,365*(k-1),dt)
 #times<-seq(t_start, 5, dt)
  
  init<-init_allZones[z,]
  # set up the init vector
  # first 5 are S of the 5 age classes, next 5 are E, 
  # next 5 are I, next 5 are L, next 5 are A, next 5 are Z
  ###############
  
  sol <- as.data.frame(ode(init_allStates, times, CyHV3_5AgeClasses_vir2pt1_dem2pt3, parameters, method="lsoda"))
  output_all<-rbind(output_all, sol)
  sol_last<-output_all[dim(output_all)[1],]
  #sol[dim(sol)[1],]
 
# # plot to see adult population output.
#  S_adults<-output_all$`3`+output_all$`4`+output_all$`5`
#  E_adults<-output_all$`8`+output_all$`9`+output_all$`10`
#  I_adults<-output_all$`13`+output_all$`14`+output_all$`15`
#  L_adults<-output_all$`18`+output_all$`19`+output_all$`20`
#  A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
#  Z_adults<-output_all$`28`+output_all$`29`+output_all$`30`


 # quartz()
 # par(mfrow=c(2,2), oma=c(0,0,2,0))
 # plot(output_all$time, S_adults , type='l', lwd=2)
 # lines(output_all$time, E_adults, col=2, lty=2, lwd=2)
 # legend(1, max(S_adults), legend=c("S", "E"), lty=c(1,2), col=c(1,2), lwd=2)
 # 
 # plot(output_all$time, I_adults , type='l', lwd=2, main = "I")
 # 
 # plot(output_all$time, L_adults , type='l', lwd=2)
 # lines(output_all$time, Z_adults, col=2, lty=2, lwd=2)
 # legend(1, max(L_adults), legend=c("L", "Z"), lty=c(1,2), col=c(1,2), lwd=2)
 # 
 # plot(output_all$time, A_adults , type='l', lwd=2, main="A")
 # 
 # T_adults<-S_adults+E_adults+I_adults+L_adults+Z_adults

#  quartz()
#  plot(output_all$time,T_adults , type='l', lwd=2, lty=1, ylim=c(0, max( max(T_adults),as.numeric(parameters$A_zt) )))
#  abline(h=parameters$A_zt, lty=2, col=2, lwd=2)