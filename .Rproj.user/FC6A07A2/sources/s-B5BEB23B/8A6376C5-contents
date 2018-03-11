#Lab 1 - Forecast Methods
  ###Chapter 1
#Task 1 - Creating Time Series
#Creating data, loading it as ts, defining the start and frequency
library(fpp)
temp<-c(7.6,7.1,8.3,11.5,13.7,17.2,18.5,19.7,15.1,8.9,8.5,8.5,7.7,6.9,6.1,10.5,12.9)
ts.temp<-ts(temp, start=2014, frequency = 12)
print(ts.temp)

#Returning start, end and frequency
start(ts.temp)
end(ts.temp)
frequency(ts.temp)

#Task 2 - Plot data
#Make a time plot of the data. Is there any time pattern in the temperature readings?

plot.ts(window(ts.temp, start=2014, end=2015), type="b", xlab="Date", ylab= "Temperatures",  
        useSmoothScatter = TRUE, connectPoints = TRUE  )
abline(h=mean(ts.temp), col='#c0d630', lwd=2.5, label="Mean")
text1<-c("Average Temperature")
legend("right",text,col="#c0d630",lty=1,ncol=1,cex=0.6,lwd=2.5)
title("Plot of Temperature")

#What is your best estimative for the average temperature in June 2015 (six months from now on)
plot.ts(window(ts.temp, start=2014, end=2015),type="b",xlab="Date",ylab="Temperatures", 
        xlim=c(2014,2016)) #Plot graph from 2014 to 2016 
abline(h=mean(ts.temp), col='#c0d630', lwd=2.5, label="Mean") # the mean value of the time series (as before)
lines(snaive(window(ts.temp, start=2014, end=2015), h=18) $mean, col="darkgrey", lwd=3 ) #this calculates and plot the next 18 forecast months
text<-c("Seasonal naive method")
legend("top", inset= .2, text,col="darkgrey",lty=1,ncol=1,cex=0.6,lwd=3)


  ###Chapter 2
#Task: Ploting time series: for each ds below, build a graph and describe main features.
#For Australian monthly electricity production from 1956 to 1995 (elec data):

library(fma)
plot.ts(window(elec, start=1956, end=1995), type="b", xlab="Date", 
        ylab="Australian Monthly Electricity Production", col="#c0d630")

monthplot(elec,ylab="Australian monthly electricity production",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: Australian monthly electricity production")
axis(1,at=1:12,labels=month.abb,cex=0.2)

#For Monthly total of people on unemployed benefits in Australia (January 1956 to July 1992) (dole):
plot.ts(window(dole, start=1956, end=1992), type="o", col='#c0d630', 
        ylab="unemployed benefits in Australia", xlab="Date")


#For Monthly total of accidental deaths in the United States (January 1973 to December 1978) (usdeaths data).
plot.ts(window(usdeaths, start=1973, end=1978), type="o", pch=10, xlab="Date", 
        ylab="Monthly acidental deaths in US")


#For production of bricks (in millions of units) at Portland, Australia (March 1956 to September 1994) (bricksq)

plot.ts(window(bricksq, start=1956, end=1994), type="o", xlab="Date", pch=20,  ylab="Production of bricks")

#seasonal plotting
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(wesanderson)){
  install.packages("wesanderson")
  library(wesanderson)
}

seasonplot(bricksq, ylab="Production of bricks per Quarter", xlab="Time", year.labels = TRUE, year.labels.left=TRUE, type="l", cex= 0.6, pch =19, lwd= "1.5", col=wes_palette(n=4, name="Moonrise2"))


##to be continued...

