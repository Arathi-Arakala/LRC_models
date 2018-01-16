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

#function to set demography parameters and density dependent mortality for model 2.3
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
#################################################################
################ virus parameters ###############################
parameters_virus <- c(beta=0.0001,
                f_1=0.8,
                f_2=0.2,
                sigma=0.001,
                xi=0.2,
                a=16.5,
                b=11.5,
                c=0,
                w_1=8.11,
                w_2= 25.15,
                w_3=9.75
)



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
    w_1<-8.11
    w_2<-25.15
    w_3<-9.75
    water_temp <- A + B*cos( (2*pi*t/365) + C)
    gamma <- w_3*exp(-(water_temp/w_2)^w_1)*water_temp^(w_1-1)*w_1*w_2^(-w_1)
    eta <- (w_3/4)*exp(-(water_temp/w_2)^w_1)*water_temp^(w_1-1)*w_1*w_2^(-w_1)
    
    
    
    quartz()
    par(mfrow=c(2,2), oma=c(0,0,2,0))
    plot(t, y[start_index:end_index], type='l', lwd=2, lty=1, col=1, ylab = "temperature")
    lines(t, pred, lwd=2, lty=2, col=3)
    abline(h=16, col=3, lwd=2, lty=2)
    abline(h=23, col=3, lwd=2, lty=2)
    
    plot(t, water_temp, type='l', lwd=2, lty=1, col=1, ylab="fitted temp")
    plot(t, gamma, type='l', lwd=2, lty=1, col=1, ylab="Gamma")
    plot(t, eta, type='l', lwd=2, lty=1, col=1, ylab="Eta")
    title(paste("Zone", z,": y=",round(A,2),"+",round(B,2), "*cos((2*pi*t/365) + ",round(C,2),")", sep=" "), outer=TRUE)
    
  }
  if(plotFlag==1){
       
  }
  
   
  
  list(A=A, B=B, C=C)
  
  
}
#cosineParams<-getFittedTempCurve(zone, 1, 21,1)

#function to scale the beta matrix by the zone area in hectares. 
# Zone Area taken from confluence site 
scaleBeta<-function(beta_mx, zone){
  Area<-c(5906,16052, 2097, 35124, 6255, 6631, 20353, 53777)
  beta_mx_new<-beta_mx/Area[zone]
  beta_mx_new
}


### function to set the demog 2.3 n virus parameters for model 2.1 ###########
#### look at confluence site (activity 3.1) for this ######################
#### Divide the total predicted age 1 carp by the model by A_z_t to scale c1 #################
set_parameters_model_vir2pt1_dem2pt3<-function(parameters_demog, parameters_virus, zone, month, SF){
 
 #fit temp curve to the BBN data for the zone
 cosineParams<-getFittedTempCurve(zone, 1, 21,0) # start year =1, number of years = 21, noPlot
 parameters_virus["a"]<-as.numeric(cosineParams$A)
 parameters_virus["b"]<-as.numeric(cosineParams$B)
 parameters_virus["c"]<-as.numeric(cosineParams$C)
 
 #set up beta_mx
 #SF<-100
 beta_mx<-matrix(0, nrow=5, ncol=5)
 beta_mx[1, 1:2]<-0.0001*SF
 beta_mx[2, 1:2]<-0.0001*SF
 beta_mx[1,3:5]<-1e-06*SF
 beta_mx[2, 3:5]<-1e-06*SF
 beta_mx[3:5,1]<-1e-06*SF
 beta_mx[3:5,2]<-1e-06*SF
 beta_mx[3:5,3:5]<-1e-04*SF
 
 #beta_mx<-1*beta_mx # added on 18 Dec 2017 to vary beta
 beta_mx<-scaleBeta(beta_mx, zone)
 
 #aggregation efffect on adults only
 if(BBN_input_virusModel$S_z_t[month]==1)
   beta_mx[3:5, 3:5]<-beta_mx[3:5, 3:5]*10
 
 #removing infection by Adults on YoY and Juv
 beta_mx[1:2, 3:5]<-0
 beta_mx[3:5, 1:2]<-0
 
 #Adding the monthly Adult popuation predicted by BBN
 A_z_t_pred<-data_A$adult_carp_number[month]
 #A_z_t_pred<-0
 
 parameters_model_vir2pt1_dem2pt3<-list(
   beta_mx=beta_mx,
   #copy all other parameters_virus
   a=parameters_virus["a"],
   b=parameters_virus["b"],
   c=parameters_virus["c"],
   f_1=parameters_virus["f_1"],
   f_2=parameters_virus["f_2"],
   w_1=parameters_virus["w_1"],
   w_2=parameters_virus["w_2"],
   w_3=parameters_virus["w_3"],
   sigma=parameters_virus["sigma"],
   xi=parameters_virus["xi"],
   
   #assign demography parameters
   mu_mx=as.matrix(parameters_demog[1:5]),
   F_mx=as.matrix(c(0,0,parameters_demog[6:8])),
   c_mx=as.matrix(parameters_demog[9:10]),
   A_zt=A_z_t_pred,
   sens=parameters_demog["sens"]
)
}

#params<-set_parameters_model2pt1(parameters_demog, parameters_virus, 1)

### Demography and virus combined differential equations #######################
source("CyHV3_5AgeClasses_vir2.1_dem2.3.R")