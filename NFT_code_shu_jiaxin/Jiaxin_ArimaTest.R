rm(list = ls())
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)
Data1 <- read.csv(file = 'spike simulation 1 - Sheet1.csv')

##Simulation1
ggtsdisplay(Data1[,1],main=c("Simulated Data 1"))
#the data is not stationary, so we take the difference of the time series until
#we get a stationary time series
DiffS1<-diff(Data1[,1], differences = 1)
ggtsdisplay(DiffS1)
#PACF plot drops off at lag d=1, then use an AR(n) model
fitS1 <- Arima(DiffS1, order=c(1,1,0))
ggtsdisplay(residuals(fitS1))
#however, we still have "spikes" in  low-order lag in the residual ACF and PACF plot.
fitS1 <- Arima(DiffS1, order=c(1,1,1))
fitS1 <- Arima(DiffS1, order=c(2,1,1))
fitS1 <- Arima(DiffS1, order=c(2,1,0))
#we found order=c(1,1,1) has the lowest AIC and BIC, so we use order=c(1,1,1)
ggtsdisplay(residuals(fitS1))

#Kernel Density Estimator
ResidualsfitS1<-residuals(fitS1)
dS1<-density(ResidualsfitS1,bw = "nrd")
ggtsdisplay(ResidualsfitS1)
plot(dS1, lwd = 2, main = "Residual Kernel Density Estimator (Simulation)",xlim=c(-100, 50))



##Real1
ggtsdisplay(Data1[,2],main=c("Real Data 1"))
#take the difference of the time series
DiffR1<-diff(Data1[,2], differences = 1)
ggtsdisplay(DiffR1)
#PACF plot drops off at lag d=1, then use an AR(n) model
fitR1 <- Arima(DiffR1, order=c(1,1,0))
ggtsdisplay(residuals(fitR1))
#however, we still have "spikes" in  low-order lag in the residual ACF and PACF plot.
fitR1 <- Arima(DiffR1, order=c(1,1,1))
fitR1 <- Arima(DiffR1, order=c(2,1,1))
fitR1 <- Arima(DiffR1, order=c(2,1,0))
#we found order=c(2,1,1) has the lowest AIC and BIC, so we use order=c(2,1,1)
ggtsdisplay(residuals(fitR1))
#Kernel Density Estimator
RfitR1<-residuals(fitR1)
dR1<-density(RfitR1,bw = "nrd")
plot(dR1, lwd = 2, main = "Residual Kernel Density Estimator (Real)",xlim=c(-100, 50))
