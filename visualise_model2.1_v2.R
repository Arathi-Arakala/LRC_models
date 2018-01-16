
############ the output will be stored in output_all
############ consolidate all disease classes into the age classes first.
A_0<-output_all$`1`+output_all$`6`+output_all$`11`+output_all$`16`+output_all$`21`+output_all$`26`
A_1<-output_all$`2`+output_all$`7`+output_all$`12`+output_all$`17`+output_all$`22`+output_all$`27`
A_2<-output_all$`3`+output_all$`8`+output_all$`13`+output_all$`18`+output_all$`23`+output_all$`28`
A_3<-output_all$`4`+output_all$`9`+output_all$`14`+output_all$`19`+output_all$`24`+output_all$`29`
A_4<-output_all$`5`+output_all$`10`+output_all$`15`+output_all$`20`+output_all$`25`+output_all$`30`



quartz()
par(mfrow=c(2,2), oma=c(0,0,2,0))
ylimit<-max( max(rowSums(cbind(A_3, A_4))), (A_0)/100)
plot(1994.333+output_all$time/365, A_3+A_4, type="l", lwd=2, col=1, ylab="Carp Numbers", xlab="year", ylim=c(0,ylimit))
lines(1994.333+output_all$time/365, (A_0)/100, type="l", col=3)#, main="Juveniles", ylab="Juvenile Fish", xlab="Year")
legend(1995, ylimit, c("Mature adult carp (3+)","Age 0 carp (100s of)"), lty=c(1,1,1), lwd=c(2.5,1),col=c("black", "green"))

ylimit<-max(max(A_1), max(A_2) )
plot(1994.333+output_all$time/365, A_1, type="l", lwd=1, col=2, main="Predicted Dynamics", ylab="", xlab="year", ylim=c(0,ylimit) ) #lines(1994.333+output_all$time/365, output_all$A_1, type="l", lwd=1, col=2)
lines(1994.333+output_all$time/365, A_2, type="l", lwd=1, col=4)
legend(1995, ylimit, c("Age 1 carp","Age 2 carp"), lty=c(1,1,1), lwd=c(2.5,1),col=c(2, 4))

ylimit<-max( 10*wetland[3:21] )

time_discrete <- seq(1995, 2013, 1)
plot(time_discrete+0.25, 10*wetland[3:21], type="h", col = "red", lwd = 3, ylim=c(0,ylimit), xlab="year", ylab="wetland area available")  #lines(calendar_year+0.2, s_spawning_area, type="h", col = "blue", lwd = 3)
legend(1995, ylimit, c( "Wetland Area (tenths of hectares)"), lty=c(1), lwd=c(2.5),col=c("red"))

title(paste("Sub-catchment Prediction Zone ",zone, sep=""), outer=TRUE)

  
############## plot number of S, E, I, A, L and Z  adult carp ##########

S_adults<-output_all$`3`+output_all$`4`+output_all$`5`
E_adults<-output_all$`8`+output_all$`9`+output_all$`10`
I_adults<-output_all$`13`+output_all$`14`+output_all$`15`
L_adults<-output_all$`18`+output_all$`19`+output_all$`20`
A_adults<-output_all$`23`+output_all$`24`+output_all$`25`
Z_adults<-output_all$`28`+output_all$`29`+output_all$`30`

virus_start<-(((VirusYear-1)*365)+1)
range_days<-virus_start:(virus_start+365)
A_max<-max(A_adults) # max number of ailing fish in a day.
maxDay<-output_all$time[which(A_adults==A_max)][1] #Day when maximum fish die.
totalDeath<-sum(A_adults[range_days])
endOfPrimaryInfection<-VirusDay+(which(A_adults[maxDay:(virus_start+365) ]<1)[1])


# quartz()
# par(mfrow=c(2,2), oma=c(0,0,2,0))
# ylimit<-max(max(S_adults), max(E_adults))
# plot(1994.333+output_all$time/365, S_adults, type="l", lwd=2, col=1, ylab="S and E Adult Carp", xlab="year", ylim=c(0,ylimit))
# lines(1994.333+output_all$time/365, E_adults, type="l", col=3)
# legend(1995, ylimit, c("Susc","Exposed"), lty=c(1,1), lwd=c(2.5,2.5),col=c(1,3))
# 
# ylimit<-max(max(I_adults), max(Z_adults))
# plot(1994.333+output_all$time/365, I_adults, type="l", lwd=2, col=2,lty=1, ylab="Infected Adult Carp", xlab="year", ylim=c(0,ylimit))
# lines(1994.333+output_all$time/365, Z_adults, type="l", col=2, lty=2, lwd=2)
# legend(1995, ylimit, c("Infected","Secondary Infected"), lty=c(1,2), lwd=c(2.5,2.5),col=c(2,2))
# 
# ylimit<-max(L_adults)
# plot(1994.333+output_all$time/365, L_adults, type="l", lwd=2, col=4,lty=1, ylab="Latent Adult Carp Numbers", xlab="year", ylim=c(0,ylimit))
# 
# ylimit<-max(A_adults)
# plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=5,lty=1, ylab="Ailing Adult Carp Numbers", xlab="year", ylim=c(0,ylimit))
# 
# title(paste("Sub-catchment Prediction Zone ",zone, sep=""), outer=TRUE)


######### zooming into ailing Adult Carp ################
# quartz()
# par(mfrow=c(1,1), oma=c(0,0,2,0))
# plot(1994.333+output_all$time/365, A_adults, type="l", lwd=2, col=5,lty=1, ylab="Ailing Adult Carp Numbers", xlab="year", ylim=c(0,ylimit/100))
# title(paste("Sub-catchment Prediction Zone ",zone, sep=""), outer=TRUE)

# #zoom into the virus release year
# quartz()
# par(mfrow=c(2,2), oma=c(0,0,2,0))
# ylimit<-max(max(S_adults[range_days]), max(E_adults[range_days]))
# plot(1994.333+(range_days)/365, S_adults[range_days], type="l", lwd=2, col=1, ylab="S and E Adult Carp", xlab="year", ylim=c(0,ylimit))
# lines(1994.333+(range_days)/365, E_adults[range_days], type="l", col=3)
# legend((1994+range_days)[1], ylimit, c("Susc","Exposed"), lty=c(1,1), lwd=c(2.5,2.5),col=c(1,3))
# 
# ylimit<-max(max(I_adults), max(Z_adults))
# plot(1994.333+(range_days)/365, I_adults[range_days], type="l", lwd=2, col=2,lty=1, ylab="Infected Adult Carp", xlab="year", ylim=c(0,ylimit))
# lines(1994.333+(range_days)/365, Z_adults[range_days], type="l", col=2, lty=2, lwd=2)
# legend((1994+range_days)[1], ylimit, c("Infected","Secondary Infected"), lty=c(1,2), lwd=c(2.5,2.5),col=c(2,2))
# 
# ylimit<-max(L_adults)
# plot(1994.333+(range_days)/365, L_adults[range_days], type="l", lwd=2, col=4,lty=1, ylab="Latent Adult Carp Numbers", xlab="year", ylim=c(0,ylimit))
# 
# ylimit<-max(A_adults)
# plot(1994.333+(range_days)/365, A_adults[range_days], type="l", lwd=2, col=5,lty=1, ylab="Ailing Adult Carp Numbers", xlab="year", ylim=c(0,ylimit), main=paste("Maximum adults=", round(A_max), sep=""), sub=paste("Infection Period is Day", VirusDay, "to", endOfPrimaryInfection, sep=" ") )
# abline(v=1994.33+VirusDay/365, col=10, lty=2)
# abline(v=1994.33+endOfPrimaryInfection/365, col=10, lty=2)
# title(paste("Virus Release Year in Zone ",zone, sep=""), outer=TRUE)
# 




