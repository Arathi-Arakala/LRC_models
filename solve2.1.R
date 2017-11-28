

  dt <- 1
  times <- seq(t_start,365*(k-1),dt)
  
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
  parameters<-set_parameters_model2pt1(parameters_demog, parameters_virus, zone)
  
  dt <- 1
  t <- seq(t_start,365*(k-1),dt)
 # times<-seq(t_start, 100, 0.001)
  
  init<-init_allZones[z,]
  # set up the init vector
  # first 5 are S of the 5 age classes, next 5 are E, 
  # next 5 are I, next 5 are L, next 5 are A, next 5 are Z
  ###############
  init_allStates<-c(as.numeric(init), rep(0, times=25))
  init_allStates[11]<-100
  sol <- as.data.frame(ode(init_allStates, times, CyHV3_5AgeClasses, parameters, method="adams"))
  output_all<-rbind(output_all, sol)
  sol_last<-output_all[dim(output_all)[1],]
  sol[dim(sol)[1],]
  # # if(k==20){
    Adults<-output_all$`13`+output_all$`14`+output_all$`15`
    quartz()
    plot(output_all$time, Adults , type='l', lwd=2)
  # # }
  # 
  # 
  # A_1<-sol_last$A_0 
  # A_2<-sol_last$A_1 
  # A_3<-sol_last$A_2
  # A_4<-sol_last$A_3 +sol_last$A_4 # new adults will be old adults plus incoming adults to this class, as adults live for 30 years.
  # 
  # A_0<-L[1,] %*% as.matrix(c(0 ,A_1, A_2, A_3, A_4)) * spawn_success(wetland[k+1])
  # 
  # init<-c(A_0=A_0, A_1=A_1, A_2=A_2, A_3=A_3, A_4=A_4)
  # t_start<-sol_last$time+1
  # year <- year+1
  # output_discrete<-rbind(output_discrete, c(year,init))
  # 


