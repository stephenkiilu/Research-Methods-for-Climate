
###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#              Autocorrelation and Cross correlation            #
#################                                 ###############
###################---------------------------###################

rm(list = ls()) 
##setwd('C:/Users/Babatunde Abiodun/Documents/Academic/Lectures/WASCAL/Codes/TimeSeries') # please change  to your 
# preferred working directory

# please install psych package by removing the # before the install commands below
install.packages('forecast')
# install.packages('TSA')
# install.packages('biwavelet') 

library(forecast)
library(TSA)
library(biwavelet)
library(zoo)

## Data importation
setwd("~/AIMS/REVIEW PHASE/RESEARCH METHODS FOR CLIMATE/Assignments/Data")
Data1 <- read_csv("Data1.csv")
time<-read.csv('time.csv')
year=Data1$Year
View(year)
## Data preparation
class(time)
dim(time)
names(time)
str(time)
anyNA(time)
View(time)
summary(time)

#####################################################################################################################################################
#QUESTION 1
#####################################################################################################################################################




needed_data <-time ## PC1 and PC2 scores
p1=time[,1]
p2=time[,2]

### converting data to time series
PC1 <- ts(needed_data$PC1, start = c(1960,1), end = c(2010,12), frequency = 12)
View(PC1)
PC2 <- ts(needed_data$PC2, start = c(1960,1), end = c(2010,12), frequency = 12)
#####
#######Question 1 (i)
par(mfcol = c(2,1))
plot(PC1, ylab = 'Monthly precipitation', main = 'Time series for PC1', col='tomato3')
lines(lowess(PC1,f=1/1))
#fit=lm(p1~year)
#pr.lm <- predict(fit)
#lines(pr.lm~year, col="black", lwd=2)
plot(PC2, ylab = 'Monthly precipitation', main = 'Time series for PC2', col='tomato3')
lines(lowess(PC2,f=1/1))
#fit1=lm(p2~year)
#summary(fit1)
#pr.lm <- predict(fit1)
#lines(pr.lm~year, col="black", lwd=2)


#########
############ Detrending
detrend1=diff(needed_data$PC1  ,differences = 1)
detrend2=diff(needed_data$PC2  ,differences = 1)
PC1_detrend=ts(detrend1, start = c(1960,1), end = c(2010,12), frequency = 12)
PC2_detrend=ts(detrend2, start = c(1960,1), end = c(2010,12), frequency = 12)

#####Plotting detrended Question 1(ii)
par(mfcol = c(2,1))
plot(PC1, ylab = 'Monthly precipitation', main = 'Time series for PC1', col='tomato3')
lines(lowess(PC1,f=1/1))
plot(PC1_detrend, ylab = 'Monthly precipitation', main = 'Time Series for detrended PC1', col='tomato3')
lines(lowess(PC1_detrend,f=1/1))
#fit=lm(p1~year)
#pr.lm <- predict(fit)
#lines(pr.lm~year, col="black", lwd=2)
###########################################
plot(PC2, ylab = 'Monthly precipitation', main = 'Time series for PC2 ', col='tomato3')
lines(lowess(PC2,f=1/1))
plot(PC2_detrend, ylab = 'Monthly precipitation', main = 'Time Series for detrended PC2', col='tomato3')
lines(lowess(PC2_detrend,f=1/1))
#fit1=lm(p2~year)
#summary(fit1)
#pr.lm <- predict(fit1)
#lines(pr.lm~year, col="black", lwd=2)


##################Question 1(iii)
############### correlogram
par(mfcol = c(1,2))
forecast::Acf(PC1_detrend, lag.max = 36,main="Autocorrelation  for detrended PC1")
forecast::Acf(PC2_detrend, lag.max = 36,main="Autocorrelation  for detrended PC2")


###########Question 1(iii)
######cross-correlation plot
#png('cross_correlation.png')
par(mfcol = c(1,1))
forecast::Ccf(PC1_detrend,PC2_detrend,main="cross-correlation for detrended PC1 &detrended PC2 ")
dev.off()
#####################################################################################################################################################
#QUESTION 2

#####################################################################################################################################################
###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#                         Fourier Analysis                      #
#################                                 ###############

#################################################### Question 2(i)
###################---------------------------################### 
detrend1=diff(needed_data$PC1  ,differences = 1)
detrend2=diff(needed_data$PC2  ,differences = 1)
PC1_detrend=ts(detrend1, start = c(1960,1), end = c(2010,12), frequency = 12)
PC2_detrend=ts(detrend2, start = c(1960,1), end = c(2010,12), frequency = 12)

####################3

png('power_spectrum_Freq.png')
par(mfcol = c(1,2))
p1 <- periodogram(PC1_detrend )
p <- periodogram(PC2_detrend)
dev.off()

## converting the frequency to periods
#png('power_spectrum_period.png')  #periodogram
############################  Question 2(ii)
period_PC1 <- 1/p1$freq
period_PC2 <- 1/p$freq

par(mfcol = c(1,2))
plot(period_PC1, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'periodogram PC1')

plot(period_PC2, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'periodogram of PC2')
dev.off()

###################---------------------------###################


#################                                 ############### Question 2(iii)
#                       Time Series Analysis                    #
#             Wavelet Analysis and Wavelet Coherence            #
#################                                 ############### wavelet analysis

###################---------------------------###################

## Wavelet Motlet option
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,PC1_detrend),dj=0.1,mother="morlet",max.scale=16) #wavelet
wavelet1=wt(cbind(timelong,PC2_detrend),dj=0.1,mother="morlet",max.scale=16) #wavelet

## plot of wavelet
#png('wavelet.png')
par(mfcol = c(1,2))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1')
#png('steve.png')
#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet1,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrend PC2')
dev.off()

#########################################################################################

###Comparing Morlet and Paul
### 
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,PC1_detrend),dj=0.1,mother="morlet",max.scale=16) #wavelet
wavelet2=wt(cbind(timelong,PC1_detrend),dj=0.1,mother="paul",max.scale=16) #wavelet
wavelet3=wt(cbind(timelong,PC1_detrend),dj=0.1,mother="dog",max.scale=16) #wavelet
############## plot

par(mfcol = c(2,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1 (Morlet option)')

#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet2,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1 (Paul option)')

#########################################################

########  Comparing Morlet and Gaussian
#####Plot
par(mfcol = c(2,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1 (Morlet option)')

#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet3,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1 (Gaussian option)')
############################################################################
        ## END #

par(mfcol = c(2,1))
plot(period_PC1, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'periodogram PC1')
#par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrended PC1')
#################################################################################################
par(mfcol = c(2,1))
plot(period_PC2, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'power',
     main = 'periodogram of PC2')

yy=plot(wavelet1,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='detrend PC2')


######################## Wavelet coherence
par(mfrow = c(2,1))
wcoh=wtc(cbind(PC1,PC2),cbind(PC1,PC2),dj=0.1,mother="morlet",max.scale=16) 
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1) #To allow for colour bar to be included
plot(wcoh,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
     lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=T,ylab="Period(Year)", xlab = "Time(Year)",
     main='Wavelet Coherence') #plotting
forecast::Ccf(PC1_detrend,PC2_detrend,main="cross-correlation for detrended PC1 &detrended PC2 ")


################################3





# Arrows pointing to the right mean that x and y are in phase.

#Arrows pointing to the left mean that x and y are in anti-phase.

#Arrows pointing up mean that y leads x by π/2.

#Arrows pointing down mean that x leads y by π/2. 


