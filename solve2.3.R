

t_start<-1
for(k in 2:(runtime-1)){     ##Note we start k at 2 as that corresponds to 1994.
  #for(k in 16:20){
  dt <- 1
  times <- seq(t_start,365*(k-1),dt)
  
  #print(init)
  
  # calculate wetland change scaling factor
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
  if(BBN_input_demogModel$A_z_t[k]==0)
    sensitivity<-50
  if(BBN_input_demogModel$A_z_t[k]>0)
    sensitivity<-sum(init[-1])/BBN_input_demogModel$A_z_t[k]
  
    
    
  parameters<-set_parameters_model2pt3(wetland_change, strength, L, sensitivity)
  
  sol <- as.data.frame(ode(init, times, demography_5classes_model2pt3, parameters))
  output_all<-rbind(output_all, sol)
  sol_last<-output_all[dim(output_all)[1],]
  
  # if(k==20){
  #   Adults<-output_all$A_2+output_all$A_3
  #   quartz()
  #   plot(output_all$time, Adults , type='l', lwd=2)
  # }


  A_1<-sol_last$A_0 
  A_2<-sol_last$A_1 
  A_3<-sol_last$A_2
  A_4<-sol_last$A_3 +sol_last$A_4 # new adults will be old adults plus incoming adults to this class, as adults live for 30 years.
  
  A_0<-L[1,] %*% as.matrix(c(0 ,A_1, A_2, A_3, A_4)) * spawn_success(wetland[k+1])
  
  init<-c(A_0=A_0, A_1=A_1, A_2=A_2, A_3=A_3, A_4=A_4)
  t_start<-sol_last$time+1
  year <- year+1
  output_discrete<-rbind(output_discrete, c(year,init))
  
}


