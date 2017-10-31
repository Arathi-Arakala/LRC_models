

t_start<-1
for(k in 2:(runtime-1)){     ##Note we start k at 2 as that corresponds to 1994.
  
  dt <- 1
  times <- seq(t_start,365*(k-1),dt)
  
  print(init)
  
  if (wetland[k]==0){
    wetland_change <-- 1
  }
  else  {
    wetland_change <- wetland[k+1]/wetland[k]
    if (wetland_change>1) wetland_change <- 1
    wetland_change <- (wetland_change)^(1/q)
  }
  
  parameters<-set_parameters(wetland_change, strength, L)
  
  sol <- as.data.frame(ode(init, times, demography_5classes, parameters))
  output_all<-rbind(output_all, sol)
  sol_last<-output_all[dim(output_all)[1],]
  
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


