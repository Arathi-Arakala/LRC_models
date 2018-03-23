
### Set of plotting function ###########

plot_demography<-function(output_all, zone, wetland){
  ############ consolidate all disease classes into the age classes first.
  A_0<-output_all$`1`+output_all$`6`+output_all$`11`+output_all$`16`+output_all$`21`+output_all$`26`
  A_1<-output_all$`2`+output_all$`7`+output_all$`12`+output_all$`17`+output_all$`22`+output_all$`27`
  A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
  A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
  A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`
  
  quartz()
  par(mfrow=c(3,2), oma=c(0,0,2,0))
  
  plot(2000+output_all$time/365, (A_0)/100, type="l", lwd=2, col=3, main="Age 0", ylab="Juvenile Fish (in 100s)", xlab="Year", ylim=c(0,max(A_0/100)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  plot(2000+output_all$time/365, A_1, type="l", lwd=2, col=2, main="Age 1 ", ylab="", xlab="Year", ylim=c(0,max(A_1)) ) 
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  plot(2000+output_all$time/365, A_2, type="l", lwd=2, col=4, main="Age 2 ", ylab="", xlab="Year", ylim=c(0,max(A_2)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  plot(2000+output_all$time/365, A_3, type="l", lwd=2, col=1, main="Age 3", ylab="", xlab="Year", ylim=c(0,max(A_3)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  plot(2000+output_all$time/365, A_4, type="l", lwd=2, col=1, main="Age 4", ylab="", xlab="Year", ylim=c(0,max(A_4)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  plot(2000+output_all$time/365, A_3+A_4, type="l", lwd=2, col=1, main="Age 3 and 4", ylab="", xlab="Year", ylim=c(0,max(A_3+A_4)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  
  # plot(2000:2016, wetland, type='l', col="blue", lty=2, lwd=2)
  # plot(2000:2016, spawn_success(wetland), col="darkblue", lty=2, lwd=2)
  
  title(paste("Sub-catchment Prediction Zone ",zone, sep=""), outer=TRUE)
  
  
}


getRecruitment<-function(Pars, State ){
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
  A_zt<-Pars$A_zt
  sens<-Pars$sens
  sigma_dem<-Pars$sigma_dem
  t_max1<-Pars$t_max1
  t_max2<-Pars$t_max2
  beta_dem0<-Pars$beta_dem0
  beta_dem1<-Pars$beta_dem1
  W_zt<-Pars$W_zt
  
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
  
  N<-rep(0, times=length(S))
  for(i in 1:5)
    N[i]<-S[i]+E[i]+I[i]+L[i]+A[i]+Z[i]
  
  #set up recruitment parameters
  t1<-(t+182)%%365
  t2<-(t+182)%%365
  if(t1==0) t1<-365
  if(t2==0) t2<-365
  var1<- ((t1-t_max1)^2)/(2*sigma_dem^2)
  var2<-((t2-t_max2)^2)/(2*sigma_dem^2)
  phi<-(1/sqrt(2*pi)) * (1/sigma_dem)* ( (0.6*exp(-var1)) + (0.4*exp(-var2)) )
  births<-(1/( (1+exp(-beta_dem0 - ( beta_dem1*W_zt))) ))*phi * ( (F_mx[4]*N[4]) + (F_mx[5]*N[5]) )
  
  births
}

# b<-rep(0, times=365*2)
# for(t in 1:(365*2) ){
#   b[t]<-getRecruitment(parameters, init_allStates)
# }
# quartz()
# plot((1:length(b))/7, b , type='l', lwd=2)
# 

plotDiseaseClasses<-function(output_all, zone, wetland){
  ############## plot number of S, E, I, A, L and Z  adult carp ##########
  
  S_adults<-output_all$`3`+output_all$`4`+output_all$`5`
  E_adults<-output_all$`8`+output_all$`9`+output_all$`10`
  I_adults<-output_all$`13`+output_all$`14`+output_all$`15`
  L_adults<-output_all$`18`+output_all$`19`+output_all$`20`
  A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
  Z_adults<-output_all$`28`+output_all$`29`+output_all$`30`
  
  stopDay<-((w_stop_index+1)+84)*7 # 84 weeks since the (w_stop_index+1)i.e. one and half years. 
#  if(stopDay>length(A_adults)) stopDay<-length(A_adults)
  # w_stop_index is the last week in the year previous to virus release
  # w_stop_index+1 is the first week of virus release year. We scan 78 weeks from there. Times 7, will give us the stop day.
  stopIndex<-which(output_all$time==stopDay)[1]
  if(is.na(stopIndex)) stopIndex<-length(A_adults)

  
    
  VirusDay# is the virus release day of the year, where day 1 is the first day of year 2000.
  #virus_start<-(((VirusYear-1)*365)+1) #to get the start day in the year range beginning in 2000, helps with plotting
  startIndex<-which(A_adults>0)[1]
  
  A_max<-max(A_adults[startIndex:(stopIndex)]) # max number of ailing fish in a day in first season
  maxIndex<-which(A_adults==A_max)[1]
  maxDay<-output_all$time[maxIndex][1] #Day when maximum fish die, where day 1 is first day of year 2000
  
  NumDaysToEnd<-(which(A_adults[maxIndex:length(A_adults) ]<1)[1])
  endIndex<-maxIndex+NumDaysToEnd
  endOfPrimaryInfection<-maxDay+NumDaysToEnd# Day when no more ailing fish.
  
  totalDeath<-sum(A_adults[startIndex:(stopIndex)])
  #timeToPeak<-maxIndex-startIndex
  timeToPeak<-maxDay-VirusDay
  
  quartz()
  par(mfrow=c(3,2), oma=c(0,0,2,0))
  
  plot(2000+output_all$time/365, S_adults, type="l", lwd=2, col=3, main="S", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(S_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  
  plot(2000+output_all$time/365, E_adults, type="l", lwd=2, col=3, main="E", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(E_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  plot(2000+output_all$time/365, I_adults, type="l", lwd=2, col=3, main="I", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(I_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  plot(2000+output_all$time/365, L_adults, type="l", lwd=2, col=3, main="L", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(L_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  plot(2000+output_all$time/365, A_adults, type="l", lwd=2, col=3, main="A", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(A_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  plot(2000+output_all$time/365, Z_adults, type="l", lwd=2, col=3, main="Z", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(Z_adults)))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(endOfPrimaryInfection/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  
  spawnindex<-which(spawnSuit_allZones[zone,]==1)-(w_stop_index+1)
  quartz()
  par(mfrow=c(1,1), oma=c(0,0,2,0))
  xrange<-2000+output_all$time/365
  plot(2000+output_all$time/365, A_adults, type="l", lwd=2, col=3, main="A", ylab="Numbers of fish", xlab="Year", ylim=c(0,max(A_adults)), xlim=c(2013,2017))
  abline(v=2000+(VirusDay/365), col="red", lty=2)
  abline(v=2000+(stopDay/365), col="red", lty=2)
  abline(v=2000+(maxDay/365), col="orange", lty=2)
  text(x=2000+(maxDay/365), y=A_max, paste("max death =", round(A_max/5,2), sep=""), pos=2)
  text(x=2014, y=1, paste("time to maximum death =", timeToPeak, "days", sep=" "), pos=2)
  text(x=2014, y=A_max, paste("total death =", round(totalDeath/5,2), sep=""), pos=2)
  title(paste("beta SF = ", SF, "virus week = ", VirusWeek, sep=" "), outer=TRUE)
  abline(v=2013+(spawnindex/52), col="brown", lty=4, lwd=2)
}

plotHeatMap<-function(input_matrix, title){
  
  mat_data<-input_matrix
  # creates a own color palette from red to green
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 149)
  
  min_data<-as.numeric(quantile( mat_data)[1])
  max_data<-as.numeric(quantile(mat_data)[5])
  break_data<-as.numeric(quantile(mat_data)[2:4])
  # # (optional) defines the color breaks manually for a "skewed" color transition
  col_breaks = c(seq(min_data,break_data[1] ,length=50),  # for red
                 seq(break_data[1]+0.1,break_data[2],length=50),           # for yellow
                seq(break_data[2]+0.1,break_data[3],length=50))             # for green

  # # creates a 5 x 5 inch image
  # png("/Users/Arathi/Documents/2018/RMIT/Research - CARP/CARP/LRC_models/OutputData/heatmap.png",    # create PNG for the heat map
  #     width = 5*300,        # 5 x 300 pixels
  #     height = 5*300,
  #     res = 300,            # 300 pixels per inch
  #     pointsize = 8)        # smaller font size

  quartz()
  heatmap.2(mat_data,
            #cellnote = round(mat_data),  # same data set for cell labels
            main = title, # heat map title
            #notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,9),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
           #breaks=col_breaks,    # enable color transition at specified limits
            dendrogram="none", # no row dendrogram
            Rowv = "NA",
            Colv="NA")            # turn off column clustering
  
 # dev.off()               # close the PNG device  
}

plotVirus<-function(op_adultCarp, VirusZone_range, S_range, output_time){
  Area<-c(5906,16052, 2097, 35124, 6255, 6631, 20353, 53777)
  carpWeight<-5 # in kgs
  
 # y_max<-(max(op_adultCarp)/2097)*carpWeight #using smallest area in den
  quartz()
  par(mfrow=c(4,2), oma=c(0,0,2,0))
  for(v_z in 1:length(VirusZone_range)){
    for(S in 1:length(S_range)){
      y_max<-(max(op_adultCarp[v_z,S, ]/Area[v_z]))*carpWeight
      if(S==1)
       plot(2000+(output_time/365), (op_adultCarp[v_z,S, ]/Area[v_z])*carpWeight , type='l', lwd=1, col=S, main=paste("Zone ", VirusZone_range[v_z], sep=""), ylab="(carp kg/ha)", xlab="time", ylim=c(0, y_max))
      if(S>1)
        lines(2000+(output_time/365), (op_adultCarp[v_z,S, ]/Area[v_z])*carpWeight, lwd=1, col=S, lty=S)
    }
    abline(v=VirusYear, col="brown")
   # legend(x=2014, y=max(op_adultCarp[v_z,,]/Area[v_z]), legend=as.character(S_range), col=1:length(S_range) )
  }
  
  
  
  # quartz()
  # plot(2000+(output_all$time/365),Total_adults, type='l', lwd=2,   )
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#library(scales)

plotAdults_ggplot<-function(op_adultCarp, VirusZone_range, S_range, output_time, VirusDay){
  library(ggplot2)
  
  
  Area<-c(5906,16052, 2097, 35124, 6255, 6631, 20353, 53777)
  carpWeight<-0.005 # in tonnes
  maxCarpInd<-which(op_adultCarp==max(op_adultCarp), arr.ind=TRUE)[1,1]
  y_max<-(max(op_adultCarp)/Area[maxCarpInd])*carpWeight #using smallest area in den
  
  f1<-NULL
  f2<-NULL
  f3<-NULL
  f4<-NULL
  f5<-NULL
  f6<-NULL
  f7<-NULL
  f8<-NULL
 
  for(v_z in 1:length(VirusZone_range)){
    zone_op_adultCarp<-t(op_adultCarp[v_z,,])
    df_adultCarp<-data.frame(t=2000+(output_time/365), y1=(zone_op_adultCarp[,1]/Area[v_z])*carpWeight, y2=(zone_op_adultCarp[,2]/Area[v_z])*carpWeight, y3=(zone_op_adultCarp[,3]/Area[v_z])*carpWeight, y4=(zone_op_adultCarp[,4]/Area[v_z])*carpWeight, y5=(zone_op_adultCarp[,5]/Area[v_z])*carpWeight )
 
   f<-ggplot( df_adultCarp,aes(x=t))+ theme_grey(base_size = 15)+
     theme(axis.text=element_text(size=10))+
     geom_line(aes(x=t, y=y1, color=1), col=1, linetype=1)+
     geom_line(aes(x=t, y=y2, color=2), col=2, linetype=2)+
     geom_line(aes(x=t, y=y3, color=3), col=3, linetype=3)+
     geom_line(aes(x=t, y=y4, color=4), col=4, linetype=4)+
     geom_line(aes(x=t, y=y5, color=5), col=5, linetype=5)+
     geom_vline(xintercept = 2000+(VirusDay/365), color="brown", lwd=0.3, lty=2)+
     labs(x="time", y="Carp (tonnes/ha/day)" )+
     scale_color_manual(name="", 
                        values = c("y1"="#00ba38", "y2"="#00ba38", "y3"="#00ba38", "y4"="#00ba38", "y5"="#00ba38")) +  # line color
     ggtitle(paste("Zone ", v_z, sep=""))+
     scale_y_continuous(limits = c(0, y_max))
   
   if(v_z==1) f1<-f
   if(v_z==2) f2<-f
   if(v_z==3) f3<-f
   if(v_z==4) f4<-f
   if(v_z==5) f5<-f
   if(v_z==6) f6<-f
   if(v_z==7) f7<-f
   if(v_z==8) f8<-f
   
     
  }
  #plot the 8 zones in a grid
  quartz()
  multiplot(f1, f2,f3, f4, cols=2)
  quartz()
  multiplot(f5, f6,f7, f8, cols=2)
  
}