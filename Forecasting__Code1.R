install.packages("forecast")
install.packages("ggplot2")
install.packages("readxl")
library(forecast)
library(ggplot2)
library(readxl)
options(digits=5, scipen=10)


setwd("C:/Users/nshanbhag/Desktop/BigDataAnalytics/Forecasting Reexam")
library(readxl)

data <- read_excel("DataSets.xlsx", sheet="Fatalities_m")
min(data)

Fatalities <- ts(data[,2], frequency = 12, start = 2001,end = 2017)
BoxCox.lambda(Fatalities)


#Split into train and test
FatalitiesTrain <- window(Fatalities, start = c(2001,01),end=c(2015,12))
#from January 2001 up to December 2015
FatalitiesTest <- window(Fatalities, start=c(2016,1),end=c(2017,12))
h <- length(FatalitiesTest)

plot(Fatalities, lwd=2, main='Evolution of the Fatalities in Belgium  between Jan. 2001 and Dec. 2017',
     xlab='Time',ylab='Fatalities Index')
lines(FatalitiesTest, col='red', lwd=2)
legend('bottomright',legend=c('Train','Test'),col=c('black','red'),lty=c(1,1), cex=0.5)


# To investage the seasonality I will used the following tools:

ggseasonplot(FatalitiesTrain,year.labels=T, year.labels.left=T)+
  ylab('No of Fatalities ')+
  ggtitle('Seasonal plot of the Road Fatalities in Belgium')


ggsubseriesplot(Fatalities)+
  ylab('Fatalities ')+
  ggtitle('Road Fatalities in Belgium')

#Autocorrelation graph

Fatalities2 <- window(Fatalities, start=2001)
ggAcf(Fatalities2, lag=48)

#ACF confirm seasonality (lag = 12) and the trend

#############################################################################



#2. Create Forecast Using naive method
#The seasonal naive method is the seasonal naïve method.
#The seasonal naive method ses each forecast to be equal to the last observed value from the
#same season of the year.




#Building the model
snaiveFatalities <- snaive(FatalitiesTrain,h=h)

#Accuracy of the model
accuracysnaiveFatalities <- accuracy(snaiveFatalities,FatalitiesTest)[,c(2,3,5,6)]
accuracysnaiveFatalities
#RMSE test = 12.26

#Visualization of the estimation
plot(Fatalities, lwd=2, main='Road Fatalities in Belgium',
     xlab='Time',ylab='Fatalities')
lines(FatalitiesTest, col='red', lwd=2)
lines(snaiveFatalities$mean, col='blue',lwd=2)
legend('topleft',legend=c('Train','Test (real observations)','Seasonal Naïve'),col=c('black','red','blue'),lty=c(1,1,1), cex=0.5)

#Check the residuals
residual1snaive <- residuals(snaiveFatalities)
checkresiduals(snaiveFatalities)
#P-value is  (p-value = 0.000042)  of the Ljung-Box test is <.05. Therefore, the residual aren't white noise. 

#Forecasting for the newt to year (h=3*12) till 2020
snaiveFatalitiesfinal <- snaive(Fatalities, h=36)
plot(snaiveFatalitiesfinal)



###########################################################################################

#3. Forecasting using the STL decomposition
#Let's have a look to the decomposition of the data
fit2 <- stl(FatalitiesTrain,t.window = 15, s.window="periodic", robust=T)
plot(fit2)
fit2adj <- seasadj(fit2)


#Trend therefore I chose to do random walk drift
#Building the model
fit2rwd <- forecast(fit2, method='rwdrift', h=h)
plot(fit2rwd)

#Visualization of the estimation
plot(rwf(fit2adj, drift=T, h=h),col='red', lwd=2)
lines(Fatalities, col='black', lwd=2)
lines(fit2rwd$mean, col='green', lwd=2)
legend("topleft", lty=1, col=c("black", "red", "blue", "green"),
       legend=c("Time series","Seasonally adjusted series",
                "Seasonally adjusted forecast", "Final forecast"), cex=0.5)
#doesn't seem to take into account the multiplicativity of
# the seasonality.

#Accuracy of the estimation
Accuracy1STL <- accuracy(fit2rwd,FatalitiesTest)
Accuracy1STL[,c(2,3,5,6)]
#RMSE test = 13.27, better than the seasonal naive

#Check the residuals
checkresiduals(fit2rwd)
#Not white noise, since the p.value < .05

#Final forecasting for the next 3 eyars
fit2_final = stl(Fatalities, t.window=15,s.window="periodic", robust=T)
FatalitiesAdj <- seasadj(fit2_final)

fit2final <- forecast(fit2_final, method='rwdrift',h=36)
plot(fit2final)



################################################################################################
################################################################################################

#4. Forecasting using Holt=Winter's
#As said in the first point, it seems that the seasonlaity is decreasing with the years.
#That's why I will only do the multiplicativity of the seasonlity. 
#Since the trend looks to be constant, I don't think that th damped parameter is usefull here
#However I'll test it to confirm this hypothesis. Therefore I'll build 4 different holt winter's
#??? modelq

#Buildint the four mdoel
hw1 <- hw(FatalitiesTrain, seasonal="multiplicative", h=h)
hw2 <- hw(FatalitiesTrain, seasonal="multiplicative",damped=T, h=h)
hw3 <- hw(FatalitiesTrain, seasonal="multiplicative", exponential=T, h=h)
hw4 <- hw(FatalitiesTrain, seasonal="multiplicative", exponential=T,damped=T,h=h)
hw5 <- hw(FatalitiesTrain, seasonal="additive", h=h)
hw6 <- hw(FatalitiesTrain, seasonal="additive", h=h,damped=T)
#Visualisation of the estimation
plot(hw1)
lines(Fatalities)
plot(hw2)
lines(Fatalities)
plot(hw3)
lines(Fatalities)
plot(hw4)
lines(Fatalities)
plot(h5)
#Visualization on the same graph
plot(Fatalities,main="Belgian Road Fatalities ", ylab="Fatalities",xlab="Year",xlim=c(2015,2017),ylim = c(30,145))
lines(hw1$mean,col=2)
lines(hw2$mean, col=3)
lines(hw3$mean,col=4)
lines(hw4$mean,col=5)


legend("topleft",lty=1,col=c(1,2,3,4,5),
       legend=c("Real Observation","Holt Winter","Damped","Exponential","Exponential Damped"),
       cex=0.5)


#Accuracy of the four models
#Train set
a_hw1 <- accuracy(hw1,FatalitiesTest)[,c(2,3,5,6)][1,]
a_hw2 <- accuracy(hw2,FatalitiesTest)[,c(2,3,5,6)][1,]
a_hw3 <- accuracy(hw3,FatalitiesTest)[,c(2,3,5,6)][1,]
a_hw4 <- accuracy(hw4,FatalitiesTest)[,c(2,3,5,6)][1,]
a_hw5 <- accuracy(hw5,FatalitiesTest)[,c(2,3,5,6)][1,]
a_hw6 <- accuracy(hw6,FatalitiesTest)[,c(2,3,5,6)][1,]


acc <- rbind(a_hw1,a_hw2,a_hw3,a_hw4,a_hw5,a_hw6)
rownames(acc) <- c("Train_HW", "Train_HW_Damped", "Train_HW_Exp", "Train_HW_Exp_Damp"
                   ,"Train_Additive_NonDamped","Train_Additive_Damped")
acc

#Test set
a_hw1 <- accuracy(hw1,FatalitiesTest)[,c(2,3,5,6)][2,]
a_hw2 <- accuracy(hw2,FatalitiesTest)[,c(2,3,5,6)][2,]
a_hw3 <- accuracy(hw3,FatalitiesTest)[,c(2,3,5,6)][2,]
a_hw4 <- accuracy(hw4,FatalitiesTest)[,c(2,3,5,6)][2,]
a_hw5 <- accuracy(hw5,FatalitiesTest)[,c(2,3,5,6)][2,]
a_hw6 <- accuracy(hw6,FatalitiesTest)[,c(2,3,5,6)][2,]


acc <- rbind(a_hw1,a_hw2,a_hw3,a_hw4,a_hw5,a_hw6)
rownames(acc) <- c("Test_HW", "Test_HW_Damped", "Test_HW_Exp", "Test_HW_Exp_Damp","Train_Additive_NonDamped","Train_Additive_Damped")
acc

#Check residual of the best model
checkresiduals(hw1)
#p value higher significant, so it's till not white noise

#Forecasting in the next two years
hwFatalitiesfinal <- hw(Fatalities, h=36,seasonal="multiplicative")
plot(hwFatalitiesfinal)




############################################################################
###########



#5. Forecasting using ETS


#Building the model
e1 <- ets(FatalitiesTrain, model ='MMM', damped=F)
e2 <- ets(FatalitiesTrain, model='MAM', damped=F)
#To be sure, I'will also do the analysis for the damped version of these model
e3 <- ets(FatalitiesTrain, model ='MMM', damped=T)
e4 <- ets(FatalitiesTrain, model='MAM', damped=T)
e5 <- ets(FatalitiesTrain, model ='MNM', damped=F)
e6 <- ets(FatalitiesTrain, model ='MNA', damped=F)


fe1 <- forecast(e1, h=h)
fe2 <- forecast (e2, h=h)
fe3 <- forecast(e3, h=h)
fe4 <- forecast (e4, h=h)
fe5 <- forecast (e5, h=h)
fe6 <- forecast (e6, h=h)

#Visualization of the forecasts
plot(Fatalities,main="Belgian Road Fatalities ", ylab="Fatalities ",xlab="Year", xlim=c(2015,2017))
lines(fe1$mean,col=2, lwd=1)
lines(fe2$mean, col=3, lwd=1)
lines(fe3$mean,col=4, lwd=1)
lines(fe4$mean,col=5)
lines(fe5$mean,col=6)
lines(fe6$mean,col=7)

legend("topleft",lty=1,col=c(1,2,3,4,5,6,7),
       legend=c("Real Observation","MMM","MAM","MMdM","MAdM","MNM","MNA"), cex=0.5)
#From the graph the best one looks to be the red curve (MMM)

#Accuracy 
#Train
ae1 <- accuracy(fe1, FatalitiesTest)[,c(2,3,5,6)][1,]
ae2 <- accuracy(fe2,FatalitiesTest)[,c(2,3,5,6)][1,]
ae3 <- accuracy(fe3, FatalitiesTest)[,c(2,3,5,6)][1,]
ae4 <- accuracy(fe4,FatalitiesTest)[,c(2,3,5,6)][1,]
ae5 <- accuracy(fe5,FatalitiesTest)[,c(2,3,5,6)][1,]
ae6 <- accuracy(fe6,FatalitiesTest)[,c(2,3,5,6)][1,]

acc <- rbind(ae1,ae2, ae3, ae4,ae5,ae6)
rownames(acc) <- c("MMM", "MAM", "MMdM", "MAdM","MNM","MNA")
acc



#Check the residual of the best model
checkresiduals(fe4)
#Not white noise

#Compare with auto ets procedure
auto_ets <- ets(FatalitiesTrain)
auto_ets$method #The selected is MNM, #There is a difference 
#Estimation test set
f <- forecast(auto_ets, h=h)
#Accuracy of AUTO.ETS
accuracy(f, FatalitiesTest)[,c(2,3,5,6)]
#Residual of AUTO.ETS
checkresiduals(auto_ets)
# not white noise

#Forecasting for the next two year, I decided to keep the MMM model
ets <- ets(Fatalities, model='MMM')
etsFatalitiesfinal <- forecast(ets, h=24)
plot(etsFatalitiesfinal)





#6. Forecasting using ARIMA


tsdisplay(FatalitiesTrain)
ndiffs(FatalitiesTrain)
nsdiffs(diff(FatalitiesTrain))
diffFatalitiestrain <- diff(FatalitiesTrain, 12)
tsdisplay(diffFatalitiestrain)







#Building, residusla and accuracy the of constructed models
m1 <- Arima(FatalitiesTrain, order = c(5,0,0), seasonal = c(0,1,1),lambda = 0)
checkresiduals(m1) 
m1$aicc
forecast.arimam1<-forecast(m1, h=h)
accTrain.m1<-accuracy(forecast.arimam1, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m1<-accuracy(forecast.arimam1, FatalitiesTest)[,c(2,3,5,6)][2,]

m2 <- Arima(FatalitiesTrain, order = c(6,0,0), seasonal = c(0,1,1),lambda = 0)
checkresiduals(m2) 
m2$aicc
forecast.arimam2<-forecast(m2, h=h)
accTrain.m2<-accuracy(forecast.arimam2, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m2<-accuracy(forecast.arimam2, FatalitiesTest)[,c(2,3,5,6)][2,]

m3 <- Arima(FatalitiesTrain, order = c(5,0,1), seasonal = c(0,1,1),lambda = 0)
checkresiduals(m3) 
m3$aicc
forecast.arimam3<-forecast(m3, h=h)
accTrain.m3<-accuracy(forecast.arimam3, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m3 <-accuracy(forecast.arimam3, FatalitiesTest)[,c(2,3,5,6)][2,]


m4 <- Arima(FatalitiesTrain, order = c(6,0,1), seasonal = c(0,1,1),lambda = 0)
checkresiduals(m4) 
m4$aicc
forecast.arimam4<-forecast(m4, h=h)
accTrain.m4<-accuracy(forecast.arimam4, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m4 <-accuracy(forecast.arimam4, FatalitiesTest)[,c(2,3,5,6)][2,]

m5 <- Arima(FatalitiesTrain, order = c(5,0,1), seasonal = c(0,1,2),lambda = 0)
checkresiduals(m5) 
m5$aicc
forecast.arimam5<-forecast(m5, h=h)
plot(forecast.arimam5)
accTrain.m5<-accuracy(forecast.arimam5, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m5 <-accuracy(forecast.arimam5, FatalitiesTest)[,c(2,3,5,6)][2,]

m6 <- Arima(FatalitiesTrain, order = c(5,0,0), seasonal = c(1,1,2),lambda = 0)
checkresiduals(m6) 
m6$aicc
forecast.arimam6<-forecast(m6, h=h)
accTrain.m6<-accuracy(forecast.arimam6, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.m6 <-accuracy(forecast.arimam6, FatalitiesTest)[,c(2,3,5,6)][2,]


#Lets do the same for auto Arima
arima.auto<-auto.arima(FatalitiesTrain, stepwise = FALSE, approximation = FALSE)
arima.auto
summary(arima.auto)
forecast.autoarima<-forecast(arima.auto, h=h)
accTrain.auto<-accuracy(forecast.autoarima, FatalitiesTest)[,c(2,3,5,6)][1,]
accTest.auto<-accuracy(forecast.autoarima, FatalitiesTest)[,c(2,3,5,6)][2,]

#Plot everything on the same graph
plot(Fatalities,main="Belgian Road Fatalities", ylab="Fatalities ",xlab="Year", xlim=c(2015,2017), ylim = c(40,145))
lines(forecast.arimam1$mean,col=2, lwd=1)
lines(forecast.arimam2$mean, col=3, lwd=1)
lines(forecast.arimam3$mean,col=4, lwd=1)
lines(forecast.arimam4$mean,col=5)
lines(forecast.arimam5$mean, col=6, lwd=1)
lines(forecast.arimam6$mean,col=7, lwd=1)
lines(forecast.autoarima$mean,col=8)

legend("topleft",lty=1,col=c(1,2,3,4,5,6,7,8),
       legend=c("Real Observation","Arima(5,0,0)(0,1,1)","Arima(6,0,0)(0,1,1)","Arima(5,0,1)(0,1,1)",
                "Arima(6,0,1)(0,1,1)","Arima(5,0,1)(0,1,2)","Arima(5,0,0)(0,1,2)","Arima(0,1,3)(2,0,0) - Auto.Arima"), cex=0.5)
#The difference between the curve are really small. 
#Just the auto.arima seems to perform worst than the other

#Accuracy measure
#Train
acc <- rbind(accTrain.m1,accTrain.m2,accTrain.m3,accTrain.m4,accTrain.m5,accTrain.m6,accTrain.auto)
rownames(acc) <- c("Arima(5,0,0)(0,1,1)","Arima(6,0,0)(0,1,1)","Arima(5,0,1)(0,1,1)",
                   "Arima(6,0,1)(0,1,1)","Arima(5,0,1)(0,1,2)","Arima(5,0,0)(0,1,2)","Arima(0,1,3)(2,0,0) - Auto.Arima")

acc

#Test
acc <- rbind(accTest.m1,accTest.m2,accTest.m3,accTest.m4,accTest.m5,accTest.m6,accTest.auto)
rownames(acc) <- c("Arima(5,0,0)(0,1,1)","Arima(6,0,0)(0,1,1)","Arima(5,0,1)(0,1,1)",
                   "Arima(6,0,1)(0,1,1)","Arima(5,0,1)(0,1,2)","Arima(5,0,0)(0,1,2)","Arima(3,0,0)(0,1,1) - Auto.Arima")

acc
#Best one is m5 ArimA(5,0,1)(0,1,2)[12], the pink line (model m5)

#Check resiudal of the best model (m5)
checkresiduals(m5)
#Not white noise

#Auto.arima give different result
checkresiduals(arima.auto)
#Not white noise
#Since they are both not white noise, I will continue with the Arima(5,0,1)(0,1,2)

#Forecasting
arima_final <- Arima(Fatalities, order=c(5,0,1),seasonal=c(0,1,2))
summary(arima_final)

arima_Final <- forecast(arima_final,h=24)
plot(arima_Final)


#7. Compare the different models
#I will compare the best model of each category in term of TEST Set performance measure
# and I will plot the estimations on a graoh to see the difference 

plot(Fatalities,main="Belgian Fatalities Index of the beverage industry Evolution", ylab="Fatalities Index",xlab="Year", xlim=c(2016,2017), ylim = c(40,145))
lines(snaiveFatalities$mean, col=2)
lines(fit2rwd$mean, col=3)
lines(hw1$mean,col=4, lwd=1)
lines(fe4$mean,col=5, lwd=1)
lines(forecast.arimam5$mean, col=6, lwd=1)
legend("topleft",lty=1,col=c(1,2,3,4,5,6,7,8),
       legend=c("Real Observations","Seasonal Naive","STL Decomposition","Holt Winter",
                "ETS (MAdM)","Arima(5,0,1)(0,1,2)"), cex=0.5)

#From the graph, it seems that the best performing model is the ETS MMM. Let's confirm that with the accuracy measures of the test set
ACCSNAIVE <- accuracysnaiveFatalities[2,]
ACCSTL <- Accuracy1STL[,c(2,3,5,6)][2,]
ACCHW <- accuracy(hw1,FatalitiesTest)[,c(2,3,5,6)][2,]
ACCETS <- accuracy(fe4, FatalitiesTest)[,c(2,3,5,6)][2,]
ACCARIMA <- accuracy(forecast.arimam5, FatalitiesTest)[,c(2,3,5,6)][2,]

acc <- rbind(ACCSNAIVE,ACCSTL,ACCHW,ACCETS,ACCARIMA)
rownames(acc) <- c("SNaive","STL Decomp.","Holt Winter", "ETS","Arima")
acc


#From the accuracy measure, the best model is the Holt Winter (lower in all the accurcy measure of the test set).
#However, as preivously said, for all the model, the residuals aren't white noise. Therefore, I can't select the
# best model according to this quality. That's why, for this exercise the selection of the model is just based on
#the accuracy maesure of the test set.

hw1 <- hw(Fatalities, seasonal="multiplicative", h=48)
Turnoverfinal <- forecast(hw1, h=48)
plot(Turnoverfinal, main='Final Prediction - using HW model')
Turnoverfinal

#References 
#. Hyndman, R.J., Athanasopoulas, G., (2018), Forecastin: Principles and Practice, available on: https:
#  //otexts.com/fpp2/
#  . Seasonal Trend Dcomposition in R (2013), available on: https://www.r-bloggers.com/seasonal-trend-decomposition-in-r/
#  . Nau, R. (2018), Statistical forecasting: notes on regression and time series analysis, available on:
#  https://people.duke.edu/~rnau/411home.htm
#. PennState Eberly College of Science (2018), Seasonal ARIMA models, available on: https:
#  //newonlinecourses.science.psu.edu/stat510/node/67/

