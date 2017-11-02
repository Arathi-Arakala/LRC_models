quartz()
plot(1994.333+output_all$time/365, rowSums(output_all[5:6]), type="l", lwd=2, col=1, main=c("Sub-catchment Prediction Zone ",zone), ylab="", xlab="year", ylim=c(0,800000))
lines(1994.333+output_all$time/365, (output_all$A_0)/100, type="l", col=3)#, main="Juveniles", ylab="Juvenile Fish", xlab="Year")

#lines(1994.333+output_all$time/365, output_all$A_1, type="l", lwd=1, col=1, main="Predicted Dynamics", ylab="", xlab="year")#, ylim=c(0,80000)) #lines(1994.333+output_all$time/365, output_all$A_1, type="l", lwd=1, col=2) 
#lines(1994.333+output_all$time/365, output_all$A_2, type="l", lwd=1, col=4)

time_discrete <- seq(1995, 2013, 1)
lines(time_discrete+0.25, 10*wetland[3:21], type="h", col = "red", lwd = 3, ylim=c(0,50000), xlab="", ylab="wetland area available")  #lines(calendar_year+0.2, s_spawning_area, type="h", col = "blue", lwd = 3) 

legend(1995, 800000, c("Mature adult carp (3+)","Age 0 carp (100s of)", "Wetland Area (tenths of hectares)"), lty=c(1,1,1), lwd=c(2.5,1,2.5),col=c("black", "green", "red")) 

#windows()
#plot(wetland[3:21], output_discrete[,2])

#windows()
#plot(rowSums(output_discrete[1:17,4:6]), output_discrete[3:19,4])



