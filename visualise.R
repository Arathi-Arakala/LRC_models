ylimit<-max( max(rowSums(output_all[5:6])), (output_all$A_0)/100)

quartz()
par(mfrow=c(2,1), oma=c(0,0,2,0))
plot(1994.333+output_all$time/365, output_all$A_3+output_all$A_4, type="l", lwd=2, col=1, ylab="Carp Numbers", xlab="year", ylim=c(0,ylimit))
lines(1994.333+output_all$time/365, (output_all$A_0)/100, type="l", col=3)#, main="Juveniles", ylab="Juvenile Fish", xlab="Year")
legend(1995, ylimit, c("Mature adult carp (3+)","Age 0 carp (100s of)"), lty=c(1,1,1), lwd=c(2.5,1),col=c("black", "green")) 

#lines(1994.333+output_all$time/365, output_all$A_1, type="l", lwd=1, col=1, main="Predicted Dynamics", ylab="", xlab="year")#, ylim=c(0,80000)) #lines(1994.333+output_all$time/365, output_all$A_1, type="l", lwd=1, col=2) 
#lines(1994.333+output_all$time/365, output_all$A_2, type="l", lwd=1, col=4)
ylimit<-max( 10*wetland[3:21] )

time_discrete <- seq(1995, 2013, 1)
plot(time_discrete+0.25, 10*wetland[3:21], type="h", col = "red", lwd = 3, ylim=c(0,ylimit), xlab="year", ylab="wetland area available")  #lines(calendar_year+0.2, s_spawning_area, type="h", col = "blue", lwd = 3) 
legend(1995, ylimit, c( "Wetland Area (tenths of hectares)"), lty=c(1), lwd=c(2.5),col=c("red")) 

title(paste("Sub-catchment Prediction Zone ",zone, sep=""), outer=TRUE)
#windows()
#plot(wetland[3:21], output_discrete[,2])

#windows()
#plot(rowSums(output_discrete[1:17,4:6]), output_discrete[3:19,4])



