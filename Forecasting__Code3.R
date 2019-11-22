install.packages("forecast")
install.packages("ggplot2")
install.packages("readxl")
library(forecast)
library(ggplot2)
library(readxl)
options(digits=5, scipen=10)


setwd("C:/Users/nshanbhag/Desktop/BigDataAnalytics/Forecasting Reexam")
library(readxl)

data <- read.csv("candy_production.csv")
min(data)

candy <- ts(data[,2], frequency = 12, start = c(1972,01),end = c(2017,08))
str(candy)
BoxCox.lambda(candy)


#Split into train and test
candyTrain <- window(candy, start=c(1972,1), end=c(2009,12))

candyTest <- window(candy, start=c(2010,1))
h <- length(candyTest)

plot(candy, lwd=2, main='Evolution of the Candy Production in U.S.',
     xlab='Time',ylab='Candy Production')
lines(candyTest, col='red', lwd=2)
legend('bottomright',legend=c('Train','Test'),col=c('black','red'),lty=c(1,1), cex=0.5)


# To investage the seasonality I will used the following tools:

ggseasonplot(candyTrain,year.labels=T, year.labels.left=T)+
  ylab('No of candy ')+
  ggtitle('Seasonal plot of the  candy production in U.S')


ggsubseriesplot(candy)+
  ylab('candy ')+
  ggtitle('candy production in U.S')



#############################################################################



#2. Create Forecast Using naive method
#The seasonal naive method is the seasonal naïve method.
#The seasonal naive method ses each forecast to be equal to the last observed value from the
#same season of the year.




#Building the model
snaivecandy <- snaive(candyTrain,h=h)

#Accuracy of the model
accuracysnaivecandy <- accuracy(snaivecandy,candyTest)[,c(2,3,5,6)]
accuracysnaivecandy
#RMSE test = 12.26

#Visualization of the estimation
plot(candy, lwd=2, main='candy production in U.S',
     xlab='Time',ylab='candy')
lines(candyTest, col='red', lwd=2)
lines(snaivecandy$mean, col='blue',lwd=2)
legend('topleft',legend=c('Train','Test (real observations)','Seasonal Naïve'),col=c('black','red','blue'),lty=c(1,1,1), cex=0.5)

#Check the residuals
residual1snaive <- residuals(snaivecandy)
checkresiduals(snaivecandy)
#P-value is  ( p-value <2e-16)  of the Ljung-Box test is <.05. Therefore, the residual aren't white noise. 

#Forecasting for the newt to year (h=3*12) till 2020
snaivecandyfinal <- snaive(candy, h=36)
plot(snaivecandyfinal)



###########################################################################################

#3. Forecasting using the STL decomposition
#Let's have a look to the decomposition of the data
fit2 <- stl(candyTrain,t.window = 15, s.window="periodic", robust=T)
plot(fit2)
fit2adj <- seasadj(fit2)


#Trend therefore I chose to do random walk drift
#Building the model
fit2rwd <- forecast(fit2, method='rwdrift', h=h)
plot(fit2rwd)

#Visualization of the estimation
plot(rwf(fit2adj, drift=T, h=h),col='red', lwd=2)
lines(candy, col='black', lwd=2)
lines(fit2rwd$mean, col='green', lwd=2)
legend("topleft", lty=1, col=c("black", "red", "blue", "green"),
       legend=c("Time series","Seasonally adjusted series",
                "Seasonally adjusted forecast", "Final forecast"), cex=0.5)
#doesn't seem to take into account the multiplicativity of
# the seasonality.

#Accuracy of the estimation
Accuracy1STL <- accuracy(fit2rwd,candyTest)
Accuracy1STL[,c(2,3,5,6)]
#RMSE test = 13.27, better than the seasonal naive

#Check the residuals
checkresiduals(fit2rwd)
#Not white noise, since the p.value < .05

#Final forecasting for the next 3 eyars
fit2_final = stl(candy, t.window=15,s.window="periodic", robust=T)
candyAdj <- seasadj(fit2_final)

fit2final <- forecast(fit2_final, method='rwdrift',h=36)
plot(fit2final)
#huge variance in the final forecast + doesn't seem to take into account the multiplicativity of
# the seasonality. 


################################################################################################
################################################################################################

#4. Forecasting using Holt=Winter's


#Buildint the four mdoel
hw1 <- hw(candyTrain, seasonal="multiplicative", h=h)
hw2 <- hw(candyTrain, seasonal="multiplicative",damped=T, h=h)
hw3 <- hw(candyTrain, seasonal="multiplicative", exponential=T, h=h)
hw4 <- hw(candyTrain, seasonal="multiplicative", exponential=T,damped=T,h=h)
hw5 <- hw(candyTrain, seasonal="additive", h=h)
hw6 <- hw(candyTrain, seasonal="additive", h=h,damped=T)
#Visualisation of the estimation
plot(hw1)
lines(candy)
plot(hw2)
lines(candy)
plot(hw3)
lines(candy)
plot(hw4)
lines(candy)
plot(h5)
#Visualization on the same graph
plot(candy,main="Candy Production in U.S", ylab="candy",xlab="Year",xlim=c(2010,2017),ylim = c(60,145))
lines(hw1$mean,col=2)
lines(hw2$mean, col=3)
lines(hw3$mean,col=4)
lines(hw4$mean,col=5)
lines(hw5$mean,col=6)
lines(hw6$mean,col=7)

legend("topleft",lty=1,col=c(1,2,3,4,5,6,7),
       legend=c("Real Observation","Holt Winter","Damped","Exponential","Exponential Damped","Additive","Additive Damped"),
       cex=0.5)


#Accuracy of the four models
#Train set
a_hw1 <- accuracy(hw1,candyTest)[,c(2,3,5,6)][1,]
a_hw2 <- accuracy(hw2,candyTest)[,c(2,3,5,6)][1,]
a_hw3 <- accuracy(hw3,candyTest)[,c(2,3,5,6)][1,]
a_hw4 <- accuracy(hw4,candyTest)[,c(2,3,5,6)][1,]
a_hw5 <- accuracy(hw5,candyTest)[,c(2,3,5,6)][1,]
a_hw6 <- accuracy(hw6,candyTest)[,c(2,3,5,6)][1,]


acc <- rbind(a_hw1,a_hw2,a_hw3,a_hw4,a_hw5,a_hw6)
rownames(acc) <- c("Train_HW", "Train_HW_Damped", "Train_HW_Exp", "Train_HW_Exp_Damp"
                   ,"Train_Additive_NonDamped","Train_Additive_Damped")
acc

#Test set
a_hw1 <- accuracy(hw1,candyTest)[,c(2,3,5,6)][2,]
a_hw2 <- accuracy(hw2,candyTest)[,c(2,3,5,6)][2,]
a_hw3 <- accuracy(hw3,candyTest)[,c(2,3,5,6)][2,]
a_hw4 <- accuracy(hw4,candyTest)[,c(2,3,5,6)][2,]
a_hw5 <- accuracy(hw5,candyTest)[,c(2,3,5,6)][2,]
a_hw6 <- accuracy(hw6,candyTest)[,c(2,3,5,6)][2,]


acc <- rbind(a_hw1,a_hw2,a_hw3,a_hw4,a_hw5,a_hw6)
rownames(acc) <- c("Test_HW", "Test_HW_Damped", "Test_HW_Exp", "Test_HW_Exp_Damp","Test_Additive_NonDamped","Test_Additive_Damped")
acc

#Check residual of the best model
checkresiduals(hw5)
#p value higher significant, so it's till not white noise

#Forecasting in the next two years
hw5 <- hw(candyTrain, seasonal="additive", h=h)
hwcandyfinal <- hw(candy, h=36,seasonal="multiplicative")
plot(hwcandyfinal)




############################################################################
###########



#5. Forecasting using ETS

#From the previous holt winter and from the first observation, the damped paraemeter doesn't seems usefull

#Building the model
e1 <- ets(candyTrain, model ='MMM', damped=F)
e2 <- ets(candyTrain, model='MAM', damped=F)
#To be sure, I'will also do the analysis for the damped version of these model
e3 <- ets(candyTrain, model ='MMM', damped=T)
e4 <- ets(candyTrain, model='MAM', damped=T)
e5 <- ets(candyTrain, model ='MNM', damped=F)
e6 <- ets(candyTrain, model ='MNA', damped=F)


fe1 <- forecast(e1, h=h)
fe2 <- forecast (e2, h=h)
fe3 <- forecast(e3, h=h)
fe4 <- forecast (e4, h=h)
fe5 <- forecast (e5, h=h)
fe6 <- forecast (e6, h=h)

#Visualization of the forecasts
plot(candy,main="Candy Production in U.S ", ylab="candy Production",xlab="Year", xlim=c(2015,2017))
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
#Test
ae1 <- accuracy(fe1, candyTest)[,c(2,3,5,6)][2,]
ae2 <- accuracy(fe2,candyTest)[,c(2,3,5,6)][2,]
ae3 <- accuracy(fe3, candyTest)[,c(2,3,5,6)][2,]
ae4 <- accuracy(fe4,candyTest)[,c(2,3,5,6)][2,]
ae5 <- accuracy(fe5,candyTest)[,c(2,3,5,6)][2,]
ae6 <- accuracy(fe6,candyTest)[,c(2,3,5,6)][2,]

acc <- rbind(ae1,ae2, ae3, ae4,ae5,ae6)
rownames(acc) <- c("MMM", "MAM", "MMdM", "MAdM","MNM","MNA")
acc



#Check the residual of the best model
checkresiduals(fe2)
#Not white noise

#Compare with auto ets procedure
auto_ets <- ets(candyTrain)
auto_ets$method #The selected is MNM, #There is a difference 
#Estimation test set
f <- forecast(auto_ets, h=h)
#Accuracy of AUTO.ETS
accuracy(f, candyTest)[,c(2,3,5,6)]
#Residual of AUTO.ETS
checkresiduals(auto_ets)
# not white noise








#7. Compare the different models
#I will compare the best model of each category in term of TEST Set performance measure
# and I will plot the estimations on a graoh to see the difference 

plot(candy,main="Evolution of Candy manufacturing in U.S", ylab="candy production",xlab="Year", xlim=c(2010,2017), ylim = c(40,145))
lines(snaivecandy$mean, col=2)
lines(fit2rwd$mean, col=3)
lines(hw5$mean,col=4, lwd=1)
lines(fe2$mean,col=5, lwd=1)
legend("topleft",lty=1,col=c(1,2,3,4,5,6,7,8),
       legend=c("Real Observations","Seasonal Naive","STL Decomposition","Holt Winter",
                "ETS (MAM)"), cex=0.5)

ACCSNAIVE <- accuracysnaivecandy[2,]
ACCSTL <- Accuracy1STL[,c(2,3,5,6)][2,]
ACCHW <- accuracy(hw5,candyTest)[,c(2,3,5,6)][2,]
ACCETS <- accuracy(fe2, candyTest)[,c(2,3,5,6)][2,]

acc <- rbind(ACCSNAIVE,ACCSTL,ACCHW,ACCETS)
rownames(acc) <- c("SNaive","STL Decomp.","Holt Winter", "ETS")
acc



hw5 <- hw(candy, seasonal="additive", h=h)
Turnoverfinal <- forecast(hw5, h=48)
plot(Turnoverfinal, main='Final Prediction - using HW model')
Turnoverfinal


