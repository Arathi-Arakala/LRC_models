###############################################################################################
#### This is a set of functions used by the demography and virus model for 5 age classes #####
###############################################################################################


### Convert discrete to continuous rates #######################

#function to set parameters and density dependent mortality
set_parameters<-function(scale_surv_juv, c, Leslie){
  parameters_5classes <- c( mu_0=-log(Leslie[2,1]*scale_surv_juv)/365,
                            mu_1=-log(Leslie[3,2]*scale_surv_juv)/365,
                            mu_2=-log(Leslie[4,3])/365,
                            mu_3=-log(Leslie[5,4])/365,
                            mu_4=-log(Leslie[5,5])/365,
                            F_2=Leslie[1,3],
                            F_3=Leslie[1,4],
                            F_4=Leslie[1,5],
                            c_0=c*10,
                            c_1=c
  )
  parameters_5classes
}


### Recruitment #######################

spawn_success <- function(x){
  
  beta_0 <- log(1/99)
  beta_1 <- 0.001
  value <- 1/(1+exp(-beta_0-beta_1*x))
  value
}

#x <- seq(100, 70000, 20)
#s <- spawn_success(x)
#plot(x,s,xlab="wetland area available", ylab="spawning success")
#s <- spawn_success(w_t)
#plot(w_t,s,xlab="habitat suitability", ylab="background mortality")



### Demography differential equations #######################
demography_5classes<-function(t, init, parms) {
  with(as.list(c(init)),{
    
    mu_0<-parms["mu_0"] # YoY stage 6 weeks to < 1year
    mu_1<-parms["mu_1"] # 1 year to < 2years
    mu_2<-parms["mu_2"]
    mu_3<-parms["mu_3"]
    mu_4<-parms["mu_4"] # adults stage, >4 years.
    
    c_0<-parms["c_0"]
    c_1<-parms["c_1"]
    
    A_0<-init[1]
    A_1<-init[2]
    A_2<-init[3]
    A_3<-init[4]
    A_4<-init[5]
    
    dA_0 = -mu_0*A_0 - c_0*(A_0*sum(init))
    dA_1 = -mu_1*A_1 - c_1*(A_1*sum(init[-1]))
    dA_2 = -mu_2*A_2 
    dA_3 = -mu_3*A_3
    dA_4 = -mu_4*A_4
    
    list(c(dA_0, dA_1, dA_2, dA_3, dA_4))
  })
}

