#################################
### FORECASTING = Exxercise 2 ###
#################################
library(forecast)
library(ggplot2)
library(readxl)
library(portes)
options(digits=5, scipen=10)


#Read the Energy Excel sheet
setwd("C:/Users/nshanbhag/Desktop/BigDataAnalytics/Forecasting Reexam") # Specify you own working directory here.
data <- read_excel("DataSets.xlsx", sheet="Energy")
#Creating the new variable



Energy <- ts(data[,3], frequency = 1, start = 1990)

#Split into train and test
EnergyTrain <- window(Energy,start=1990,end=2010)
EnergyTest <- window(Energy, start=2011,end=2016)
h <- length(EnergyTest)
#Plot the entire data
plot(Energy, lwd=2, main='Evolution of the Gross Inland Consumption of Renewable Energy EU',
     xlab='Time',ylab='Gross Renewable Energy Consumption(TOE)')
lines(EnergyTest, col='red', lwd=2)

legend('bottomright',legend=c('Train','Test'),col=c('black','red'),lty=c(1,1), cex=0.5)


Energy2 <- window(Energy, start=1990)
ggAcf(Energy2, lag=24)
#The Autocorerlation plot confirm that, no seasonliaty, + trends



#######################################################################################
#######################################################################################


#2. Create Forecast Using naive method
#naive method in this case is the random walk

rwdrift <- rwf(EnergyTrain,h=h,drift=TRUE)
accuracynaiveEnergy <- accuracy(rwdrift,EnergyTest)[,c(2,3,5,6)]
accuracynaiveEnergy

plot(Energy, lwd=2, main='Evolution of the Gross Renewable Energy Consumption in EU',
     xlab='Time',ylab='Renewable Energy Consumption')
lines(EnergyTest, col='red', lwd=2)
lines(rwdrift$mean, col='blue',lwd=2)
legend('topleft',legend=c('Train','Test (real observations)','Random Walk with Drift'),col=c('black','red','blue'),lty=c(1,1,1), cex=0.5)


residual1naive <- residuals(rwdrift)
checkresiduals(rwdrift)
#P-value of the Ljung-Box test is <.001. Therefore, the residual aren't white noise. 

#########################################################################################

holt1 <- holt(EnergyTrain, h=h, initial='simple')
holt2 <- holt(EnergyTrain, h=h, exponential=T, initial='simple')
holt3 <- holt(EnergyTrain, h=h, damped=T, initial='optimal')
holt4 <- holt(EnergyTrain, h=h, exponential=T, damped=T, initial='optimal')


#Visualisation
plot(holt1)
lines(Energy)
plot(holt2)
lines(Energy)
plot(holt3)
lines(Energy)
plot(holt4)
lines(Energy)

#on the same graph
plot(Energy,main="Evolution of the Gross Renewable Energy Consumption in EU", ylab="Renewable Energy Consumption",xlab="Year",cex=0.5)
lines(holt1$mean, col=2)
lines(holt2$mean, col=3)
lines(holt3$mean, col=4)
lines(holt4$mean, col=5)
legend("topleft",lty=1,col=c(1,2,3,4,5),
       legend=c("Real Observation","Holt's Linear","Exponential","Damped","Exponential & Damped"))

#Accuracy of the four models
#Train set
a_h1 <- accuracy(holt1,EnergyTest)[,c(2,3,5,6)][1,]
a_h2 <- accuracy(holt2,EnergyTest)[,c(2,3,5,6)][1,]
a_h3 <- accuracy(holt3,EnergyTest)[,c(2,3,5,6)][1,]
a_h4 <- accuracy(holt4,EnergyTest)[,c(2,3,5,6)][1,]

acc <- rbind(a_h1,a_h2,a_h3,a_h4)
rownames(acc) <- c("Holt Linear", "Exp.", "Damped", "Exp & Damped")
acc

#Test set
a_h1 <- accuracy(holt1,EnergyTest)[,c(2,3,5,6)][2,]
a_h2 <- accuracy(holt2,EnergyTest)[,c(2,3,5,6)][2,]
a_h3 <- accuracy(holt3,EnergyTest)[,c(2,3,5,6)][2,]
a_h4 <- accuracy(holt4,EnergyTest)[,c(2,3,5,6)][2,]

acc <- rbind(a_h1,a_h2,a_h3,a_h4)
rownames(acc) <- c("Holt Linear", "Exp.", "Damped", "Exp & Damped")
acc

#According to the accuracy measures the best model is the number 3, with the damped parameter. 

# aic <- rbind(holt1$model$aic, holt2$model$aic, holt3$model$aic, holt4$model$aic)
# colnames(aic) <- c("AIC")
# rownames(aic) <- c("holt1", "holt2", "holt3", "holt4")
# aic

#Residuals
checkresiduals(holt3)
#the resiudals are white noise, p.value=.63

#####################################################################################

#4. Forecasting with ETS
modeltest <- c('AAN' ,'MAN', 'MAN')
#Wanted to test AMN but it's a unstable model

#Model construction
ets1 <- ets(EnergyTrain, model ='AAN', damped=F)
ets2 <- ets(EnergyTrain, model='MAN',damped=F)
ets3 <- ets(EnergyTrain, model ='MMN', damped=F)
ets1damped <- ets(EnergyTrain, model ='AAN', damped=T)
ets2damped <- ets(EnergyTrain, model='MAN', damped=T)
ets3damped <- ets(EnergyTrain, model ='MMN', damped=T)

fe1 <- forecast(ets1, h=h)
fe2 <- forecast (ets2, h=h)
fe3 <- forecast(ets3, h=h)
fe4 <- forecast (ets1damped, h=h)
fe5 <- forecast (ets2damped, h=h)
fe6 <- forecast (ets3damped, h=h)

#on the same graph
plot(Energy,main="Evolution of the Gross Renewable Energy Consumption in EU", ylab="Renewable Energy Consumption",xlab="Year",cex=0.5)
lines(fe1$mean, col=2)
lines(fe2$mean, col=3)
lines(fe3$mean, col=4)
lines(fe4$mean, col=5)
lines(fe5$mean, col=6)
lines(fe6$mean, col=7)
legend("topleft",lty=1,col=c(1,2,3,4,5,6,7),
       legend=c("Real Observation","AAN","MAN","MMN","AAdN","MAdN","MMdN"),cex=0.5)
#With the graph best oens seem to be either MMdN or MMN

#Accuracy 
#Train
ae1 <- accuracy(fe1,EnergyTest)[,c(2,3,5,6)][1,]
ae2 <- accuracy(fe2,EnergyTest)[,c(2,3,5,6)][1,]
ae3 <- accuracy(fe3,EnergyTest)[,c(2,3,5,6)][1,]
ae4 <- accuracy(fe4,EnergyTest)[,c(2,3,5,6)][1,]
ae5 <- accuracy(fe5,EnergyTest)[,c(2,3,5,6)][1,]
ae6 <- accuracy(fe6,EnergyTest)[,c(2,3,5,6)][1,]

acc <- rbind(ae1,ae2, ae3, ae4, ae5, ae6)
rownames(acc) <- c("AAN", "MAN", "MMN", "AAdN","MAdN","MMdN")
acc

#TEst
ae1 <- accuracy(fe1, EnergyTest)[,c(2,3,5,6)][2,]
ae2 <- accuracy(fe2,EnergyTest)[,c(2,3,5,6)][2,]
ae3 <- accuracy(fe3, EnergyTest)[,c(2,3,5,6)][2,]
ae4 <- accuracy(fe4,EnergyTest)[,c(2,3,5,6)][2,]
ae5 <- accuracy(fe5,EnergyTest)[,c(2,3,5,6)][2,]
ae6 <- accuracy(fe6,EnergyTest)[,c(2,3,5,6)][2,]

acc <- rbind(ae1,ae2, ae3, ae4, ae5, ae6)
rownames(acc) <- c("AAN", "MAN", "MMN", "AAdN","MAdN","MMdN")
acc
#Best one is the MAdN (ets2damped)

#Check the residual of the best model
checkresiduals(ets2damped)
#Not white noise, p value .15

#Compare with auto ets procedure
auto_ets <- ets(EnergyTrain)
auto_ets$method
f <- forecast(auto_ets, h=h)
accuracy(f, EnergyTest)[,c(2,6)]
checkresiduals(auto_ets)
#The model from Auto ETS is AAN so it's different, the result is white noise, but RMSE is higher

################################################################################################
################################################################################################
################################################################################################


#5. Forecasting using ARIMA
#Non stationnary data, since a clear trend
ggAcf(EnergyTrain)
ggPacf(EnergyTrain)
diffEnergytrain <- diff(EnergyTrain, 1)

tsdisplay(EnergyTrain)
tsdisplay(diffEnergytrain)
#Now the data is stationnary


#Building, residusla and accuracy the of constructed models
m1 <- Arima(EnergyTrain, order = c(0,2,1))
checkresiduals(m1) 
m1$aicc
forecast.arimam1<-forecast(m1, h=h)
accTrain.m1<-accuracy(forecast.arimam1, EnergyTest)[,c(2,3,5,6)][1,]
accTest.m1<-accuracy(forecast.arimam1, EnergyTest)[,c(2,3,5,6)][2,]

m2 <- Arima(EnergyTrain, order = c(1,2,1))
checkresiduals(m2) 
m2$aicc
forecast.arimam2<-forecast(m2, h=h)
accTrain.m2<-accuracy(forecast.arimam2, EnergyTest)[,c(2,3,5,6)][1,]
accTest.m2<-accuracy(forecast.arimam2, EnergyTest)[,c(2,3,5,6)][2,]

m3 <- Arima(EnergyTrain, order = c(1,2,0))
checkresiduals(m3) 
m3$aicc
forecast.arimam3<-forecast(m3, h=h)
accTrain.m3<-accuracy(forecast.arimam3, EnergyTest)[,c(2,3,5,6)][1,]
accTest.m3 <-accuracy(forecast.arimam3, EnergyTest)[,c(2,3,5,6)][2,]


m4 <- Arima(EnergyTrain, order = c(2,2,1))
checkresiduals(m4) 
m4$aicc
forecast.arimam4<-forecast(m4, h=h)
accTrain.m4<-accuracy(forecast.arimam4, EnergyTest)[,c(2,3,5,6)][1,]
accTest.m4 <-accuracy(forecast.arimam4, EnergyTest)[,c(2,3,5,6)][2,]


#Lets do the same for auto Arima
arima.auto<-auto.arima(EnergyTrain, stepwise = FALSE, approximation = FALSE)
arima.auto #Auto arima is 0,2,0
summary(arima.auto)
forecast.autoarima<-forecast(arima.auto, h=h)
accTrain.auto<-accuracy(forecast.autoarima, EnergyTest)[,c(2,3,5,6)][1,]
accTest.auto<-accuracy(forecast.autoarima, EnergyTest)[,c(2,3,5,6)][2,]

#Plot everything on the same graph
plot(Energy,main="Evolution of the Gross Renewable Energy Consumption in EU", ylab="Renewable Energy Consumption",xlab="Year")
lines(forecast.arimam1$mean,col=2, lwd=1)
lines(forecast.arimam2$mean, col=3, lwd=1)
lines(forecast.arimam3$mean,col=4, lwd=1)
lines(forecast.arimam4$mean,col=5)
lines(forecast.autoarima$mean,col=8)

legend("topleft",lty=1,col=c(1,2,3,4,5,6,7,"darkolivegreen1",8),
       legend=c("Real Observation","Arima(0,2,1)","Arima(1,2,1)","Arima(1,2,0)",
                "Arima(2,2,1)","Arima(0,2,0) - Auto.Arima"),cex=0.5)
#The difference between the curve are really small. 
#Just the auto.arima seems to perform worst than the other

#Accuracy measure
#Train
acc <- rbind(accTrain.m1,accTrain.m2,accTrain.m3,accTrain.m4,accTrain.auto)
rownames(acc) <- c("Arima(0,2,1)","Arima(1,2,1)","Arima(1,2,0)",
                   "Arima(2,2,1)","Arima(0,2,0) - Auto.Arima")

acc

#Test
acc <- rbind(accTest.m1,accTest.m2,accTest.m3,accTest.m4,accTest.auto)
rownames(acc) <- c("Arima(0,2,1)","Arima(1,2,1)","Arima(1,2,0)",
                   "Arima(2,2,1)","Arima(0,2,0) - Auto.Arima")

acc





checkresiduals(m3)
checkresiduals(arima.auto)

#White noise, p.value - .52

#Forecasting
arima_final <- Arima(Energy, order=c(1,1,1))
summary(arima_final)

arima_Final <- forecast(arima_final,h=3)
plot(arima_Final)


#Refereneces : https://fr.slideshare.net/21_venkat/arima-26196965
########################################################################
########################################################################


#7. Compare the different models
#I will compare the best model of each category in term of TEST Set performance measure
# and I will plot the estimations on a graoh to see the difference 

plot(Energy,main="Evolution of the Gross Renewable Energy Consumption in EU", ylab="Renewable Energy Consumption",xlab="Year")
lines(rwdrift$mean, col=2)
lines(holt3$mean, col=3)
lines(fe5$mean,col=4, lwd=1)
lines(forecast.arimam3$mean,col=5, lwd=1)
legend("topleft",lty=1,col=c(1,2,3,4,5),
       legend=c("Real Observations","Random Walk Drift","Damped Holt Method","ETS(MAdN)",
                "Arima(1,2,0)"))

#From the graph, it seems that the best performing model is the ETS MMdN or the RandomW walk drift. Let's confirm that with the accuracy measures of the test set
ACCRANDWALKDRIFT<- accuracynaiveEnergy[2,]
ACCHOLT <-accuracy(holt3,EnergyTest)[,c(2,3,5,6)][2,]
ACCETS <- accuracy(fe5,EnergyTest)[,c(2,3,5,6)][2,]
ACCARIMA <- accuracy(forecast.arimam3, EnergyTest)[,c(2,3,5,6)][2,]


acc <- rbind(ACCRANDWALKDRIFT,ACCHOLT,ACCETS,ACCARIMA)
rownames(acc) <- c("RandomW Walk Drift","Damped Holt","ETS MAdN","Arima(1,2,0")
acc

#The best one is the MMdN i will therefore do my last forecasting with this model
#The forecast of the ETS model MMM is the follwoing one: 
#Forecasting for the next two year, I decided to keep the MMM model
naiveEnergyfinal <- rwf(Energy, h=4, drift=T)

plot(naiveEnergyfinal, main='Final Prediction - using Random Walk Drift model')



