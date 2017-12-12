#########################################################
####### Epi model with demography - 5 age classes #######
#########################################################

CyHV3_5AgeClasses<-function(t, State, Pars){
  
  
  with(as.list(c(State, Pars)), {
    #get all the parameters
    beta_mx<-Pars$beta_mx
    f_1=Pars$f_1
    f_2=Pars$f_2
    sigma=Pars$sigma
    xi=Pars$xi
    a=Pars$a
    b=Pars$b
    c=Pars$c
    w_1=Pars$w_1
    w_2=Pars$w_2
    w_3=Pars$w_3
    mu_mx<-Pars$mu_mx
    F_mx<-Pars$F_mx
    c_mx<-Pars$c_mx
    sens<-Pars$sens
    
    
    S<-State[1:5]
    E<-State[(6):(10)]
    I<-State[(11):(15)]
    L<-State[ 16:20]
    A<-State[ 21:25]
    Z<- State[ 26:30]
    
    S<-as.matrix(S)
    E<-as.matrix(E)
    I<-as.matrix(I)
    L<-as.matrix(L)
    A<-as.matrix(A)
    Z<-as.matrix(Z)
    
    dS<-rep(0, times=5)
    dE<-rep(0, times=5)
    dI<-rep(0, times=5)
    dL<-rep(0, times=5)
    dA<-rep(0, times=5)
    dZ<-rep(0, times=5)
    # total population of each age class
    Total<-as.matrix(S+E+I+L+A+Z)
    
    #set up virus parameters dependent on temp
    water_temp <- a + b*cos( (2*pi*t/365) + c)
    gamma <- w_3*exp(-(water_temp/w_2)^w_1)*water_temp^(w_1-1)*w_1*w_2^(-w_1)
    eta <- (w_3/4)*exp(-(water_temp/w_2)^w_1)*water_temp^(w_1-1)*w_1*w_2^(-w_1)
    
    for(i in 1:5){
      
      if(i==1){
        dS[i]=-S[i]*(beta_mx[,i]%*%(I+ L + A  ) ) -mu_mx[i]*S[i] - c_mx[i]*(S[i]*sum(Total))
        dE[i]=S[i]*(beta_mx[,i]%*%(I+ L + A  ) )- eta*E[i] -mu_mx[i]*E[i] - c_mx[i]*(E[i]*sum(Total))
        dI[i]=eta*E[i]-gamma*I[i] -mu_mx[i]*I[i] - c_mx[i]*(I[i]*sum(Total))
        dL[i]=(1-f_1)*gamma*I[i]+(1-f_2)*gamma*Z[i]-sigma*L[i] -mu_mx[i]*L[i] - c_mx[i]*(L[i]*sum(Total))
        dA[i]=f_1*gamma*I[i] + f_2*gamma*Z[i] - xi*A[i] -mu_mx[i]*A[i] - c_mx[i]*(A[i]*sum(Total))
        dZ[i]=sigma*L[i]-gamma*Z[i] -mu_mx[i]*Z[i] - c_mx[i]*(Z[i]*sum(Total))
        
        # #only demography, no virus
        # dS[i]=-mu_mx[i]*S[i] - c_mx[i]*(Total[i]*sum(Total))
        # dE[i]=-mu_mx[i]*E[i] - c_mx[i]*(Total[i]*sum(Total))
        # dI[i]=-mu_mx[i]*I[i] - c_mx[i]*(Total[i]*sum(Total))
        # dA[i]=-mu_mx[i]*L[i] - c_mx[i]*(Total[i]*sum(Total))
        # dL[i]=-mu_mx[i]*A[i] - c_mx[i]*(Total[i]*sum(Total))
        # dZ[i]=-mu_mx[i]*Z[i] - c_mx[i]*(Total[i]*sum(Total))
        # 
        if(E[i]==0 && dE[i]<0) dE[i]<-0
        if(I[i]==0 && dI[i]<0) dI[i]<-0
        if(L[i]==0 && dL[i]<0) dL[i]<-0
        if(A[i]==0 && dA[i]<0) dA[i]<-0
        if(Z[i]==0 && dZ[i]<0) dZ[i]<-0
         }
      
      if(i==2){
        dS[i]=-S[i]*(beta_mx[,i]%*%(I+ L + A  ) ) -mu_mx[i]*S[i] - (c_mx[i]*(S[i]*sum(Total[-1]))*sens)
        dE[i]=S[i]*(beta_mx[,i]%*%(I+ L + A  ) )- eta*E[i] -mu_mx[i]*E[i] - (c_mx[i]*(E[i]*sum(Total[-1]))*sens)
        dI[i]=eta*E[i]-gamma*I[i] -mu_mx[i]*I[i] - (c_mx[i]*(I[i]*sum(Total[-1]))*sens)
        dA[i]=f_1*gamma*I[i] + f_2*gamma*Z[i] - xi*A[i] -mu_mx[i]*A[i] - (c_mx[i]*(A[i]*sum(Total[-1]))*sens)
        dL[i]=(1-f_1)*gamma*I[i]+(1-f_2)*gamma*Z[i]-sigma*L[i] -mu_mx[i]*L[i] - (c_mx[i]*(L[i]*sum(Total[-1]))*sens)
        dZ[i]=sigma*L[i]-gamma*Z[i] -mu_mx[i]*Z[i] - (c_mx[i]*(Z[i]*sum(Total[-1]))*sens)
      
        # dS[i]=-mu_mx[i]*S[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        # dE[i]=-mu_mx[i]*E[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        # dI[i]=-mu_mx[i]*I[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        # dL[i]=-mu_mx[i]*L[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        # dA[i]=-mu_mx[i]*A[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        # dZ[i]=-mu_mx[i]*Z[i] - (c_mx[i]*(Total[i]*sum(Total[-1]))*sens)
        
        if(E[i]==0 && dE[i]<0) dE[i]<-0
        if(I[i]==0 && dI[i]<0) dI[i]<-0
        if(L[i]==0 && dL[i]<0) dL[i]<-0
        if(A[i]==0 && dA[i]<0) dA[i]<-0
        if(Z[i]==0 && dZ[i]<0) dZ[i]<-0
        
        }
      if(i>2){
        dS[i]=-S[i]*(beta_mx[,i]%*%(I+ L + A  ) ) -mu_mx[i]*S[i]
        dE[i]=S[i]*(beta_mx[,i]%*%(I+ L + A  ) )- eta*E[i] -mu_mx[i]*E[i]
        dI[i]=eta*E[i]-gamma*I[i] -mu_mx[i]*I[i]
        dA[i]=f_1*gamma*I[i] + f_2*gamma*Z[i] - xi*A[i] -mu_mx[i]*A[i]
        dL[i]=(1-f_1)*gamma*I[i]+(1-f_2)*gamma*Z[i]-sigma*L[i] -mu_mx[i]*L[i]
        dZ[i]=sigma*L[i]-gamma*Z[i] -mu_mx[i]*Z[i]
      
        # dS[i]=-mu_mx[i]*S[i] 
        # dE[i]=-mu_mx[i]*E[i] 
        # dI[i]=-mu_mx[i]*I[i] 
        # dL[i]=-mu_mx[i]*L[i] 
        # dA[i]=-mu_mx[i]*A[i] 
        # dZ[i]=-mu_mx[i]*Z[i] 
        
        if(E[i]==0 && dE[i]<0) dE[i]<-0
        if(I[i]==0 && dI[i]<0) dI[i]<-0
        if(L[i]==0 && dL[i]<0) dL[i]<-0
        if(A[i]==0 && dA[i]<0) dA[i]<-0
        if(Z[i]==0 && dZ[i]<0) dZ[i]<-0
        }
      
    }
    
    return(list(c(dS, dE, dI, dL,dA, dZ)))
  })
  
  
  
}




