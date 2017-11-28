###############################################################################################
#### This is a set of functions used by the demography and virus model (2.3) for 5 age classes #####
###############################################################################################

### Recruitment #######################

spawn_success <- function(x){
  
  beta_0 <- log(1/99)
  beta_1 <- 0.001
  value <- 1/(1+exp(-beta_0-beta_1*x))
  value
}

#function to set parameters and density dependent mortality for model 2.3
set_parameters_model2pt3<-function(scale_surv_juv, c, Leslie, sensitivity){
  parameters_5classes <- c( mu_0=-log(Leslie[2,1]*scale_surv_juv)/365,
                            mu_1=-log(Leslie[3,2]*scale_surv_juv)/365,
                            mu_2=-log(Leslie[4,3])/365,
                            mu_3=-log(Leslie[5,4])/365,
                            mu_4=-log(Leslie[5,5])/365,
                            F_2=Leslie[1,3],
                            F_3=Leslie[1,4],
                            F_4=Leslie[1,5],
                            c_0=c*10,
                            c_1=c,
                            sens=sensitivity
                            
  )
  parameters_5classes
}

##function to fit temperature data to a sine wave
######################################################
# y=A+B*(cos((2*pi*t/365) + C)) 
# Use compound angle formula cos(A+B)=cosAcosB-sinAsinB
# A_0=A
# A_1=B*cos(C)
# A_2=B*sin(C)
# y=A_0+A_1*X_1+A_2*X_2
######################################################

getFittedTempCurve<-function(zone, start_year, nYears, plotFlag ){
  z<-zone
  inputfile<-paste("BBN_Data/zone_avg_water_temp-",z,"_zn.csv", sep="")
  data_T<-as.data.frame(read.csv(inputfile, header=TRUE, sep="," ))
  
  y=data_T$avg_est_watertemp
  # calculating the indices of the first days of each month over the year
  noOfDaysInAMonth<-c(31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31)
  startDay<-1
  firstDayOfMonths<-startDay
  for(i in 1:(length(noOfDaysInAMonth)-1)  ){
    nextMonth<-startDay+noOfDaysInAMonth[i]
    firstDayOfMonths<-c(firstDayOfMonths, nextMonth )
    startDay<-nextMonth
  }
  # list out the indices of first day of month for 21 years
  t_all<-numeric()
  Nyears<-21
  for(i in 1:Nyears){
    t_all<-c(t_all, firstDayOfMonths+(365*(i-1)) )
  }
  
  #select the time period
  start_index<- ((start_year-1)*12)+1
  end_index<-(start_index+(12*nYears))-1
  
  t<-t_all[start_index:end_index]
  
  X_1<-cos(2*pi*t/365)
  X_2<-sin(2*pi*t/365)
  Temp_df<-data.frame(time=t, temp=y[start_index:end_index], X1=X_1, X2=X_2 )
  
  sineWave<-lm(temp ~ time + X1+ X2, data=Temp_df)
  summary(sineWave)
  
  A_0=sineWave$coefficients[1]
  A_1=sineWave$coefficients[3]
  A_2=sineWave$coefficients[4]
  
  A<-A_0
  B<-sqrt(A_1^2 + A_2^2)
  C=atan2(A_2, A_1)
  
  pred=A+B*(cos((2*pi*t/365) + C))
  
  if(plotFlag==1){
    quartz()
    par(mfrow=c(1,1), oma=c(0,0,2,0))
    plot(t, y[start_index:end_index], type='l', lwd=2, lty=1, col=1, ylab = "temperature")
    lines(t, pred, lwd=2, lty=2, col=3)
    abline(h=16, col=3, lwd=2, lty=2)
    abline(h=23, col=3, lwd=2, lty=2)
    title(paste("Zone", z,": y=",round(A,2),"+",round(B,2), "*cos((2*pi*t/365) + ",round(C,2),")", sep=" "), outer=TRUE)
    
  }
  
  list(A=A, B=B, C=C)
  
  
}
#test function
sineParams<-getFittedTempCurve(1, 2, 2,1)
sineParams$A
sineParams$B
sineParams$C

CyHV3_pars <- c(beta=0.0015,
                f=0.8,
                f_2=0.2,
                sigma=0.0001,
                xi=2,
                a=16.5,
                b=11.5,
                w_1 <- 8.11,
                w_2 <- 25.15,
                w_3 <- 9.75
)

set_parameters_model3pt0<-function(parameters_demog, parameters_virus, zone){
  
}



### Demography differential equations #######################
demography_5classes_model2pt3<-function(t, init, parms) {
  with(as.list(c(init)),{
    
    mu_0<-parms["mu_0"] # YoY stage 6 weeks to < 1year
    mu_1<-parms["mu_1"] # 1 year to < 2years
    mu_2<-parms["mu_2"]
    mu_3<-parms["mu_3"]
    mu_4<-parms["mu_4"] # adults stage, >4 years.
    
    c_0<-parms["c_0"]
    c_1<-parms["c_1"]
    sens<-parms["sens"]
    
    A_0<-init[1]
    A_1<-init[2]
    A_2<-init[3]
    A_3<-init[4]
    A_4<-init[5]
    
    dA_0 = -mu_0*A_0 - c_0*(A_0*sum(init))
    dA_1 = -mu_1*A_1 - c_1*(A_1*sum(init[-1]))*sens
    dA_2 = -mu_2*A_2 
    dA_3 = -mu_3*A_3
    dA_4 = -mu_4*A_4
    
    list(c(dA_0, dA_1, dA_2, dA_3, dA_4))
  })
}

