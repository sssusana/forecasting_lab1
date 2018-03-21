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

##Evert dataset has seasonality, except the seasonal ones.


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


##Because is very difficult to see seasonal whithin years, we have this function
##This function has the monthly seasonality
## In each month you can see the average for each month and the trend. 
#Those are values per mmonth among the years. 

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

##5. Monthly sales of new one-family houses sold in the USA since 1973 (hsales2 data).
plot.ts(hsales2,type="o",xlab="Time",ylab="Monthly sales of new one-family houses sold in the USA", pch=20) #time series plot of temp with points market as bubbles

monthplot(hsales2,ylab="Monthly sales of new one-family houses sold in the USA",
          ab="Month",xaxt="n",
          main="Seasonal deviation plot: Monthly sales of new one-family houses sold in the
          USA")
axis(1,at=1:12,labels=month.abb,cex=0.8)

######SIMPLE FORECAST METHODS####

#Four principal models
#Mean: average of past point
#Naive method:last value 

plot.ts(window(usdeaths, start=1973, end=1978),type="b",xlab="Time",ylab="Monthly total of accidental deaths in the USA", xlim=c(1973,1980)) #time series plot of te mp with points market as bubbles
lines(meanf(window(usdeaths, start = 1973, end = 1978),h=24)$mean,col=2,lwd=3)
lines(naive(window(usdeaths, start = 1973, end = 1978),h=24)$mean,col=3,lwd=3)
lines(snaive(window(usdeaths, start = 1973, end = 1978),h=24)$mean,col=4,lwd=3)
lines(rwf(window(usdeaths, start= 1973, end = 1978),drift=T,h=24)$mean,col=5,lwd=3
)
text<-c("Mean method","Naive method", "Seasonal naive method", "Drift method")
legend("topright",text,col=2:5,lty=1,ncol=1,cex=0.5,lwd=3)

#Daily closing IBM stock prices (data set ibmclose).
plot.ts(window(ibmclose),type="b",xlab="Time",ylab="Daily closing IBM stock prices ", xlim=c(1, 379)) #time series plot of temp with points market as bubbles
lines(meanf(window(ibmclose),h=24)$mean,col=2,lwd=3)
lines(naive(window(ibmclose),h=24)$mean,col=3,lwd=3)
lines(snaive(window(ibmclose),h=24)$mean,col=4,lwd=3)
lines(rwf(window(ibmclose),drift=T,h=24)$mean,col=5,lwd=3)
text<-c("Mean method","Naive method", "Seasonal naive method", "Drift method")
legend("topright",text,col=2:5,lty=1,ncol=1,cex=0.8,lwd=3)


#######Calculating Seasonal coe cients########

install.packages("Quandl")
library(Quandl)
library(TTR)
shoes<-Quandl("BUNDESBANK/ICP_M_PT_N_0321_2_4_INX", api_key="ZtDn381y9hB7NJdEhVqF" , type="ts", start_date="1996-01-31")
plot.ts(shoes, main = "Portugal Shoes Data")

shoes_sub <- window(shoes, start=c(2005, 1), end=c(2015,12))
shoes_sub

##The function filter let you filter the dataset you're are going to use.
#We are creating a filter named = rep (1/12, 12) and sides provides 
shoes_sub_f12 <- stats::filter(shoes_sub, filter = rep(1/12, 12), sides = 1) #sum of all values between January and December, divided by 12, attributed to Dec =100 shoes_sub_f12
#Sides=2: some before and some after. 

shoes_sub_f13<-stats::filter(shoes_sub, filter = c(1/24, 1/12, 1/12,1/12, 1/12, 1/12, 1/12, 1/12, 1/12,
                                                   1/12, 1/12, 1/12,
                                                   1/24), sides = 2) #weighted average of 13 data points attributed to period 7.


shoes_sub_f12_4 <- SMA(shoes_sub, n = 12) #sum of all values between January and D ecember, divided by 12, attributed to Dec =100
shoes_sub_f12_4

shoes_sub_f13_2 <- ma(shoes_sub, order = 12) #weighted average of 13 data points a ttributed to period 7.
shoes_sub_f13_2

shoes_sub_f13_2 <- ma(shoes_sub, order = 12) #weighted average of 13 data points a ttributed to period 7.
shoes_sub_f13_2

plot.ts(cbind(shoes_sub, shoes_sub_f12, shoes_sub_f12_2, shoes_sub_f13, shoes_sub_f12_4,
              shoes_sub_f13_2 ), plot.type = 'single', col = c(1, 2, 3, 4, 5, 6))
legend("bottomleft",c("Series","MA(12)-1","MA(12)-2","MA(13) weighted", "Function
                      SMA", "Function ma"), col=1:6,lty=1,lwd=2,ncol=1,cex=0.6)


#calculating de-trended series
det_shoes_sub<-shoes_sub-shoes_sub_f13_2
#calculating seasonal coefficients
S<-c(rep(0,12)) 
for (i in 1:12){
  S[i]<-mean(det_shoes_sub[i+12*(1:12)], na.rm=TRUE)
}
S
mean (S)
sum(S)
S1 = S-mean(S)
S1
sum(S1)

shoes_ann <- Quandl("BUNDESBANK/ICP_M_PT_N_0321_2_4_INX", type="ts", start_date="1996-01-31", collapse="annual")
shoes_ann


####Seasonal coefficients####


sa<-c(284,213,227,308,262,228,236,320)
ts.sa<-ts(sa, start=2015, frequency = 4)

ts.sa

x=2.51-42.79-29
x

y=(1/8)*284+(1/4)*213+(1/4)*227+(1/4)*308+(1/8)*262
trend<-filter(ts.sa, filter = c(1/8,1/4,1/4,1/4,1/8),sides=2)
plot.ts(ts.sa)
lines(trend,col="blue")
y
# Calculate the de-trended Q3 of Year 1.
ts.sa[3]-y
# Calculate the de-trended and de-seasonalized Q3 of Year 1.
ts.sa[3]-y-(-29)

#Assume that the trend was calculated using the LOESS method. 
#The trend for Q1 of Year 1 is now 271.6. What is the random value for Q1 Year 1?
ts.sa[1]-271.66-2.51

ts.sa[1]-271.66

Assume that the trend was calculated using the following linear regression,
Trend = 220 + 4t + 3t2 . What is the trend for Q3 Year1? What is the de-trended value for Q3 Year1?
  t=3
trend = 220+4*t+3*(t)^2 trend

###Complete this!!

########Decomposition


##to be continued...

