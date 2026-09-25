# Project 3
# Team Member: Sonia, Saleh, Vineet

library(ggplot2)
library(gridExtra)
library(TSstudio)
library(zoo)
library(scales)
library(forecast)
library(imputeTS)
library(tsbox)
library(plyr)
library(lubridate)
library(scatterplot3d)


hrlyData <- read.table("hrlyData.csv",header=TRUE,sep=',')
hrlyData$Time <- as.POSIXct(hrlyData$Time,tz="",format = "%Y-%m-%d %H:%M:%S")

EasyTraining <- hrlyData[hrlyData$Time<"2021-02-02 00:00:00"& hrlyData$Time>="2020-09-01 00:00:00",c("Time","NetDemand")]
EasyTraining <- EasyTraining[c(-1,-3698),]

EasyTrainingTS <- ts(EasyTraining$NetDemand, frequency = 168)

EasyTest <- hrlyData[hrlyData$Time>="2021-02-02 00:00:00"& hrlyData$Time<"2021-02-09 00:00:00",c("Time","NetDemand")]
EasyTest <- EasyTest[c(-1,-170),]

plot(EasyTraining,main="Easy Training Set",type = "l", ylab="Net Demand",xlab="DTime")


DiffTraining <- hrlyData[hrlyData$Time>="2021-08-01 00:00:00"& hrlyData$Time<="2021-12-24 23:00:00",c("Time","NetDemand")]
DiffTraining <- DiffTraining[c(-1,-2),]
DiffTrainingTS <- ts(DiffTraining$NetDemand, frequency = 168)

DiffTest <- hrlyData[hrlyData$Time>="2021-12-25 00:00:00"&hrlyData$Time<="2021-12-31 23:00:00",c("Time","NetDemand")]
DiffTest <- DiffTest[c(-1,-2),]

plot(DiffTraining,main="Difficult Training Set",type = "l", ylab="Net Demand",xlab="Time")

#plotting the acf and pacf for easy training set
Eacf=ggAcf(EasyTraining$NetDemand,lag.max=500,main="ACF of Easy Training Set", lwd=1.5)
Epacf=ggPacf(EasyTraining$NetDemand,lag.max=500,main="PACF of Easy Training Set", lwd=1.5)
grid.arrange(Eacf,Epacf,nrow=1)

#differencing the easy training set
EdiffData = diff(diff(EasyTraining$NetDemand,lag=1),lag=168)
EdiffDataWithMonths <- data.frame(EasyTraining$NetDemand[170:3696],EdiffData)
names(EdiffDataWithMonths) = c("Month","differences")

EacfDiff=ggAcf(EdiffData,lag.max=500,main="ACF of Easy Training Set", lwd=1.5)
EpacfDiff=ggPacf(EdiffData,lag.max=500,main="PACF of Easy Training Set", lwd=1.5)
grid.arrange(EacfDiff,EpacfDiff,nrow=1)


#plotting the acf and pacf for difficult training set
Dacf=ggAcf(DiffTraining$NetDemand,lag.max=500,main="ACF of difficult Training Set", lwd=1.5)
Dpacf=ggPacf(DiffTraining$NetDemand,lag.max=500,main="PACF of difficult Training Set", lwd=1.5)
grid.arrange(Dacf,Dpacf,nrow=1)

#differencing the difficult training set
DdiffData = diff(diff(DiffTraining$NetDemand,lag=1),lag=168)
DdiffDataWithMonths <- data.frame(DiffTraining$NetDemand[170:3504],DdiffData)
names(DdiffDataWithMonths) = c("Month","differences")

DacfDiff=ggAcf(DdiffData,lag.max=500,main="ACF of Difficult Training Set", lwd=1.5)
DpacfDiff=ggPacf(DdiffData,lag.max=500,main="PACF of Difficult Training Set", lwd=1.5)
grid.arrange(DacfDiff,DpacfDiff,nrow=1)


#creating an empty data frame to keep track of our training results
# for Easy Training Set

columnNames = c("p","d","q","P","D","Q","AIC Fit","RMSE","Test MAPE")
EmodelParameters = data.frame(matrix(ncol=9,nrow=0))
colnames(EmodelParameters)=columnNames

# for Difficult training set

DmodelParameters  <- data.frame(matrix(ncol=9,nrow=0))
colnames(DmodelParameters)=columnNames

# easy training ARIMA Model

p=0
d=1
q=0
P=0
D=1
Q=0

#modelArima <- auto.arima(EasyTrainingTS,seasonal=TRUE)
#modelArima
#Emodel <- arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="CSS")
Emodel <- Arima(EasyTrainingTS,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="ML")
Emodel

ERMSE <- sqrt(mean((Emodel$residuals)^2))
Eresults <- c(p,d,q,P,D,Q,Emodel$aic,ERMSE,NA)
EmodelParameters[nrow(EmodelParameters)+1,] = Eresults

Eresiduals <-as.vector(residuals(Emodel))
EresidualsDF <- as.data.frame(Eresiduals)

#get fitted values using the fitted() function  (from forecast library)
EfittedValues<-as.vector(fitted(model))
residVsFitted <- data.frame(residuals,fittedValues)
fittedDF<-data.frame(clothingTrain$Month,fittedValues)
names(fittedDF)=c("Month","fittedValues")

# Difficult training ARIMA Model
#model1
p1=3
d1=1
q1=1
P1=0
D1=1
Q1=2

#modelArima <- auto.arima(EasyTrainingTS,seasonal=TRUE)
#modelArima
#Emodel <- arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="CSS")
Dmodel1 <- Arima(DiffTrainingTS,order=c(p1,d1,q1),seasonal=list(order=c(P1,D1,Q1),period=168),method="ML")
Dmodel1


DRMSE1 <- sqrt(mean((Dmodel1$residuals)^2))
Dresults1 <- c(p1,d1,q1,P1,D1,Q1,Dmodel1$aic,DRMSE1,NA)
DmodelParameters[nrow(DmodelParameters)+1,] = Dresults1

#model2
p2=18
d2=1
q2=1
P2=1
D2=1
Q2=2

#modelArima <- auto.arima(EasyTrainingTS,seasonal=TRUE)
#modelArima
#Emodel <- arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="CSS")
Dmodel2 <- Arima(DiffTrainingTS,order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period=168),method="ML")
Dmodel2


DRMSE2 <- sqrt(mean((Dmodel2$residuals)^2))
Dresults2 <- c(p2,d2,q2,P2,D2,Q2,Dmodel2$aic,DRMSE2,NA)
DmodelParameters[nrow(DmodelParameters)+1,] = Dresults2


#plotting residuals for the best model
#get fitted values using the fitted() function  (from forecast library)
Dresiduals <-as.vector(residuals(Dmodel))
DresidualsDF <- as.data.frame(Dresiduals)
DfittedValues<-as.vector(fitted(Dmodel))
DresidVsFitted <- data.frame(Dresiduals,DfittedValues)
DfittedDF<-data.frame(DiffTraining$Time,DfittedValues)
names(DfittedDF)=c("Time","fittedValues")

#run Box/jUng test on residuals to test for autocorrelation
#reject null hypothesis that data are independent if p-value is small (want large p-value)
DBoxResult = Box.test(Dresiduals,lag=48,fitdf=(p+q+P+Q),type="Ljung")
DBoxResult

#plot ACF/PACF of residuals
DResidAcf=ggAcf(Dresiduals,lag.max=50,main="ACF of the Residuals")
DResidPacf = ggPacf(Dresiduals,lag.max=50,main="PACF of the Residuals")
grid.arrange(DResidAcf,DResidPacf,nrow=1)

##ggplot version of 4 in 1 plots
#Normal Probability Plot
DNormProb=ggplot(DresidualsDF,aes(sample=Dresiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
DFittedVsResid = ggplot(data=DresidVsFitted, aes(x=DfittedValues,y=Dresiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
DResidHist = ggplot(DresidualsDF,aes(x=Dresiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
DResidVsOrder =ggplot(data=DresidualsDF, aes(x=rownames(DresidualsDF), y = Dresiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


grid.arrange(DNormProb, DFittedVsResid,DResidHist, DResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
DFittedVsActuals = ggplot(data=DfittedDF, aes(x=Time,y=DfittedValues))+
  geom_point()+geom_line(data=DiffTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Month",y = "Sales in Thousands ($)")
DFittedVsActuals

###***********Lets see how well our fitted model forecasts our validation set*******************
#First forecasting
DtestSetForecast1 = forecast(Dmodel,h=168)

#lets calculate our validation set forecast MAPE
DMAPEValidation1 <-mean(abs(DtestSetForecast1$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DMAPEValidation1 = round(DMAPEValidation1, digits = 3)

#Second Forecasting
DtestSetForecast2 = forecast(Dmodel,h=168)

#lets calculate our validation set forecast MAPE
DMAPEValidation2 <-mean(abs(DtestSetForecast2$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DMAPEValidation2 = round(DMAPEValidation2, digits = 3)


#Lets plot our forecast, actuals, and confidence intervals for our validation set
DfittedData<-data.frame(Time=DiffTraining$Time,NetDemand=DtestSetForecast$fitted)
DforecastValues<-data.frame(Month=DiffTest$Time,NetDemand=DtestSetForecast$mean)
DforecastUpper95<-data.frame(Month=DiffTest$Time,NetDemand=DtestSetForecast$upper[,2])
DforecastLower95<-data.frame(Month=DiffTest$Time,NetDemand=DtestSetForecast$lower[,2])

#-------------------------------Holt Winter Method-----------------------------------
#------------------------------------------------------------------------------------

#Imputation did not use in model because we did not have any missing values.
statsNA(EasyTraining$NetDemand,bins=5)
statsNA(DiffTraining$NetDemand,bins=5)

#impute the missing value in easy training set last value carried forward
imputedLVCF = na_locf(EasyTraining$NetDemand)

#lets join our imputed series back to their time values in new data frames
imputedLVCF <- data.frame(EasyTraining$Time,imputedLVCF)
colnames(imputedLVCF)<-c("Time","NetDemand")
imputedLVCF_TS <- ts(imputedLVCF$NetDemand, frequency = 168)
statsNA(imputedLVCF$NetDemand,bins=5)


hwDifficult<-HoltWinters(DiffTrainingTS,alpha=0.1, beta=0, gamma=0.9,seasonal="additive")
hwEasy<-HoltWinters(EasyTrainingTS,alpha=0.05, beta=0, gamma=0.9,seasonal="additive")


summary(hwDifficult)
plot(hwDifficult)

summary(hwEasy)
plot(hwEasy)

xDifficultTS = hwDifficult$fitted[,1]  #this retrieves the smoothed values as a time series
class(xDifficultTS)
xEasyTS = hwEasy$fitted[,1]  #this retrieves the smoothed values as a time series
class(xEasyTS)

xDifficultDF = ts_df(xDifficultTS) #converts time series object to data frame (for easier graphing)
xEasyDF = ts_df(xEasyTS) #converts time series object to data frame (for easier graphing)


#CHANGE TIMES TO BE FROM ORIGINAL TRAINING SET
xDifficultDF$time<-DiffTraining$Time[169:3504]
xEasyDF$time<-EasyTraining$Time[169:3696]

colors <- c("Actual" = "black","Winters"="blue")  #define colors for the graph to be used later
g1<-ggplot(xDifficultDF, aes(x=time,y=value,color = "Winters"))+
  labs(y="Demand", x="Date",title="Winters Smoothed Training Data for Difficult Week")+
  geom_line(size = 1)+
  geom_point(data=DiffTraining,aes(x=Time,y=NetDemand,color="Actual"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 month",date_labels = "%b-%y")+
  scale_colour_manual(values= colors)+labs(colour="Legend")
g1

colors <- c("Actual" = "black","Winters"="blue")  #define colors for the graph to be used later
g2<-ggplot(xEasyDF, aes(x=time,y=value,color = "Winters"))+
  labs(y="Demand", x="Date",title="Winters Smoothed Training Data for Easy Week")+
  geom_line(size = 1)+
  geom_point(data=imputedLVCF,aes(x=Time,y=NetDemand,color="Actual"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 month",date_labels = "%b-%y")+
  scale_colour_manual(values= colors)+labs(colour="Legend")
g2

#Difficult model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("Alpha","Beta","Gamma","RMSE","MAPE Test")
DifficultmodelResults = data.frame(matrix(ncol=5,nrow=0))
colnames(DifficultmodelResults)=columnNames

SSE = hwDifficult$SSE
MSE = hwDifficult$SSE/length(DiffTrainingTS)
print(paste("MSE=",MSE))
RMSE = sqrt(MSE)
print(paste("RMSE=",RMSE))

#Forecast Validation set for difficult
DiffTestTS <- ts(DiffTest$NetDemand, frequency = 168)
length(DiffTestTS)

DifficultValidForecast<-predict(hwDifficult,n.ahead=168,prediction.interval=TRUE)
DMAE <- mean(abs(DifficultValidForecast[,1]-DiffTest$NetDemand))
DMAPE<- mean(abs(DifficultValidForecast[,1]-DiffTest$NetDemand)/DiffTest$NetDemand)*100

#round(MSE,4),round(RMSE,4),round(DMAE,4),round(DMAPE,4)
#DifficultmodelResults[1,7]=round(DMAE,4)

DifficultmodelResults[2,5]=round(DMAPE,4)
results = c(round(hwDifficult$alpha,4),round(hwDifficult$beta,4),round(hwDifficult$gamma,4),round(RMSE,4),'Null')
DifficultmodelResults[nrow(DifficultmodelResults)+1,] = results

#Easy model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("Alpha","Beta","Gamma","RMSE","MAPE Test")
EasymodelResults = data.frame(matrix(ncol=5,nrow=0))
colnames(EasymodelResults)=columnNames

SSE = hwEasy$SSE
MSE = hwEasy$SSE/length(EasyTrainingTS)
print(paste("MSE=",MSE))
RMSE = sqrt(MSE)
print(paste("RMSE=",RMSE))

EasyTestTS <- ts(EasyTest$NetDemand, frequency = 168)
length(EasyTestTS)

EasyValidForecast<-predict(hwEasy,n.ahead=168,prediction.interval=TRUE)
EasyValidForecast[,1]<-as.numeric(EasyValidForecast[,1])
EMAE <- mean(abs(EasyValidForecast[,1]-EasyTest$NetDemand))
EMAPE<- mean(abs(EasyValidForecast[,1]-EasyTest$NetDemand)/EasyTest$NetDemand)*100

#EasymodelResults[6,7]=round(EMAE,4)
#EasymodelResults[2,5]=round(EMAPE,4)

results = c(round(hwEasy$alpha,4),round(hwEasy$beta,4),round(hwEasy$gamma,4),round(RMSE,4),'Null')
EasymodelResults[nrow(EasymodelResults)+1,] = results



##Plot the Forecast With CI Lines
#lets grab some things from our forecast we need for graphing (the ts_df turns the time series objects into regular data frames)
predictions = ts_df(EasyValidForecast[,1])
predictions$time<-EasyTest$Time

upperCI = ts_df(EasyValidForecast[,2])
upperCI$time<-EasyTest$Time

lowerCI = ts_df(EasyValidForecast[,3])
lowerCI$time<-EasyTest$Time

colors2 <- c("Actual" = "black","Forecast"="Red","95% CI"="green")  #define colors for the graph to be used later

g<-ggplot(data=EasyTest,aes(x=Time,y=NetDemand, color="Actual"))+
  labs(y="Net Demand", x="Date",title="Easy Net Demand Forecast + Holt Winter")+
  geom_line(size = 1)+
 # geom_point(data=trainingSet,aes(x=Date,y=West, color="Actual"))+
 # geom_point(data=EasyTest,aes(x=Time,y=NetDemand, color="Actual"))+
  geom_line(data=predictions,aes(x=time,y=value, color="Forecast"))+
  geom_line(data=upperCI,aes(x=time,y=value, color="95% CI"))+
  geom_line(data=lowerCI,aes(x=time,y=value, color="95% CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%b-%d")+
  scale_colour_manual(values= colors2)+labs(colour="Legend")+
  annotate(geom="text",x=EasyTest[50,1],y=4800,label=paste("MAPE=1.342"))
g


Dpredictions = ts_df(DifficultValidForecast[,1])
Dpredictions$time<-DiffTest$Time

DupperCI = ts_df(DifficultValidForecast[,2])
DupperCI$time<-DiffTest$Time

DlowerCI = ts_df(DifficultValidForecast[,3])
DlowerCI$time<-DiffTest$Time

colors2 <- c("Actual" = "black","Forecast"="Red","95% CI"="green")  #define colors for the graph to be used later

g<-ggplot(data=DiffTest,aes(x=Time,y=NetDemand, color="Actual"))+
  labs(y="Net Demand", x="Date",title="Difficult Net Demand Forecast + Holt Winter")+
  geom_line(size = 1)+
  # geom_point(data=trainingSet,aes(x=Date,y=West, color="Actual"))+
  # geom_point(data=EasyTest,aes(x=Time,y=NetDemand, color="Actual"))+
  geom_line(data=Dpredictions,aes(x=time,y=value, color="Forecast"))+
  geom_line(data=DupperCI,aes(x=time,y=value, color="95% CI"))+
  geom_line(data=DlowerCI,aes(x=time,y=value, color="95% CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%b-%y")+
  scale_colour_manual(values= colors2)+labs(colour="Legend")+
  annotate(geom="text",x=DiffTest[30,1],y=5000,label=paste("MAPE=12.7304"))
g


###################################################################################
#-----------------------------------------------------------------------------------
#------------------------Analyzing weather data-------------------------------------
#------------------------Scatter Plots for seeing relationship between data---------

WeatherData <- read.table("201806 202112 Raw Weather and RIT Calendar Data.csv",header=T,sep=",")
WeatherData$Time<- as.POSIXct(WeatherData$Time,tz="","%m/%d/%y %I:%M %p")

start <- as.POSIXct('2018-06-01 00:00:00',tz='EST')  
end <- as.POSIXct('2021-12-31 23:55:00',tz='EST')    

#Lets make a condition that returns 5 minute, 30 minute, 60 minute intervals
#Here I use 60 minutes interval
interval<-60 

if(interval==5){
  time =data.frame( seq.POSIXt(from = start, to =end , by = "5 min"))
  colnames(time)= c("Time") # name the column "time"
  HrlyWeatherData <- join(time,WeatherData, by = "Time", type = "left", match = "first")
} else if(interval==30) {
  time =data.frame( seq.POSIXt(from = start, to =end , by = "30 min"))
  colnames(time)= c("Time") # name the column "time" 
  HrlyWeatherData <- join(time,WeatherData, by = "Time", type = "left", match = "first")
} else {
  time =data.frame( seq.POSIXt(from = start, to =end , by = "60 min"))
  colnames(time)= c("Time") # name the column "time" 
  HrlyWeatherData <- join(time,WeatherData, by = "Time", type = "left", match = "first")
}

EasyTrainingWeather <- HrlyWeatherData[HrlyWeatherData$Time<"2021-02-02 00:00:00"& HrlyWeatherData$Time>="2020-11-01 00:00:00",]
EasyTrainingWeather$NetDemand <-EasyTraining$NetDemand

#EasyTrainingWeatherTS <- ts(EasyTrainingWeather$NetDemand, frequency = 168)

EasyTestWeather <- HrlyWeatherData[HrlyWeatherData$Time>="2021-02-02 00:00:00"& HrlyWeatherData$Time<"2021-02-09 00:00:00",]
EasyTestWeather$NetDemand <- EasyTest$NetDemand
#plot(EasyTrainingWeather,main="Easy Training Set",type = "l", ylab="Net Demand",xlab="DTime")

DiffTrainingWeather <- HrlyWeatherData[HrlyWeatherData$Time>="2021-08-01 00:00:00"& HrlyWeatherData$Time<="2021-12-24 23:00:00",]
DiffTrainingWeather$NetDemand <- DiffTraining$NetDemand
#DiffTrainingTS <- ts(DiffTraining$NetDemand, frequency = 168)

DiffTestWeather <- HrlyWeatherData[HrlyWeatherData$Time>="2021-12-25 00:00:00"&HrlyWeatherData$Time<="2021-12-31 23:00:00",]
DiffTestWeather$NetDemand <- DiffTest$NetDemand
#plot(DiffTraining,main="Difficult Training Set",type = "l", ylab="Net Demand",xlab="Time")

apply(EasyTrainingWeather,2, anyNA)

#impute the missing value in easy training set last value carried forward
EasyimputedLVCF = na_locf(EasyTrainingWeather)

#lets join our imputed series back to their time values in new data frames
EasyimputedLVCF <- data.frame(EasyimputedLVCF)


EasyTrainingWeather$Ev_Classes

ggplot(EasyTrainingWeather, aes(x=AW_ExtTemp, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=AW_ExtHum, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=AW_FeelsLike, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=OA_WindSpe, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=AW_DewPoint, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=OA_WindDir, y=NetDemand)) + geom_point()
ggplot(EasyTrainingWeather, aes(x=Ev_Classes, y=NetDemand)) + geom_point()

EasySemesterVsNetDemand = EasyimputedLVCF[which(EasyimputedLVCF$Semester==1),c("Semester","NetDemand")]
EasySemesterVsNetDemand$Semester <- as.factor(EasySemesterVsNetDemand$Semester)
EasySemesterVsNetDemandPlot <-ggplot(EasySemesterVsNetDemand,aes(x=Semester,y=NetDemand))+
  geom_boxplot()
EasySemesterVsNetDemandPlot


################################################################################
################################################################################
#####################Begin Indicator Variable Model ############################
#-------------------------------------------------------------------------------####
#Create Monthly Indicator Variables ####
#Make indicator variables for each month, use 11 of the months as regressors in your arima model
#Dynamic regression with indicators

monthOfYear<-data.frame(hrlyData$Time,month(hrlyData$Time))
names(monthOfYear)<-c("Date","Month")

EasyTrainingMonthOfYear <- monthOfYear[monthOfYear$Date<"2021-02-02 00:00:00"& monthOfYear$Date>="2020-09-01 00:00:00",]
EasyTestMonthofYear <- monthOfYear[monthOfYear$Date>="2021-02-02 00:00:00"& monthOfYear$Date<"2021-02-09 00:00:00",]


EasyTrainingMonthIndicator<-class.ind(EasyTrainingMonthOfYear$Month)
EasyTestMonthIndicator<-class.ind(EasyTestMonthofYear$Month)

DiffTrainingMonthOfYear <- monthOfYear[monthOfYear$Date>="2021-08-01 00:00:00"& monthOfYear$Date<="2021-12-24 23:00:00",]
DiffTestMonthofYear <- monthOfYear[monthOfYear$Date>="2021-12-25 00:00:00"&monthOfYear$Date<="2021-12-31 23:00:00",]

DiffTrainingMonthIndicator<-class.ind(DiffTrainingMonthOfYear$Month)
DiffTestMonthIndicator<-class.ind(DiffTestMonthofYear$Month)


dayOfWeek <-data.frame(hrlyData$Time,day(hrlyData$Time))
names(dayOfWeek)<-c("Date","Day")

EasyTrainingDayofWeek <- dayOfWeek[dayOfWeek$Date<"2021-02-02 00:00:00"& dayOfWeek$Date>="2020-09-01 00:00:00",]
EasyTestDayofWeek <- dayOfWeek[dayOfWeek$Date>="2021-02-02 00:00:00"& dayOfWeek$Date<"2021-02-09 00:00:00",]

EasyTrainingDayIndicator<-class.ind(EasyTrainingDayofWeek$Day)
EasyTestDayIndicator<-class.ind(EasyTestDayofWeek$Day)

DiffTrainingDayofWeek <- dayOfWeek[dayOfWeek$Date>="2021-08-01 00:00:00"& dayOfWeek$Date<="2021-12-24 23:00:00",]
DiffTestDayofWeek <- dayOfWeek[dayOfWeek$Date>="2021-12-25 00:00:00"&dayOfWeek$Date<="2021-12-31 23:00:00",]

DiffTrainingDayIndicator<-class.ind(DiffTrainingDayofWeek$Day)
DiffTestDayIndicator<-class.ind(DiffTestDayofWeek$Day)


TimeofDay <-data.frame(hrlyData$Time,hour(hrlyData$Time))
names(TimeofDay)<-c("Date","Hour")

EasyTrainingTimeofDay <- TimeofDay[TimeofDay$Date<"2021-02-02 00:00:00"& TimeofDay$Date>="2020-09-01 00:00:00",]
EasyTestTimeofDay <- TimeofDay[TimeofDay$Date>="2021-02-02 00:00:00"& TimeofDay$Date<"2021-02-09 00:00:00",]

EasyTrainingTimeIndicator<-class.ind(EasyTrainingTimeofDay$Hour)
EasyTestTimeIndicator<-class.ind(EasyTestTimeofDay$Hour)

DiffTrainingTimeofDay <- TimeofDay[TimeofDay$Date>="2021-08-01 00:00:00"& TimeofDay$Date<="2021-12-24 23:00:00",]
DiffTestTimeofDay <- TimeofDay[TimeofDay$Date>="2021-12-25 00:00:00"&TimeofDay$Date<="2021-12-31 23:00:00",]

DiffTrainingTimeIndicator<-class.ind(DiffTrainingTimeofDay$Hour)
DiffTestTimeIndicator<-class.ind(DiffTestTimeofDay$Hour)


#dayOfMonth <-data.frame(hrlyData$Time,day(hrlyData$Time))
#names(dayOfMonth)<-c("Date","Day")

#EasyTrainingDayofMonth <- dayOfMonth[dayOfMonth$Date<"2021-02-02 00:00:00"& dayOfMonth$Date>="2020-11-01 00:00:00",]
#EasyTestDayofMonth <- dayOfMonth[dayOfMonth$Date>="2021-02-02 00:00:00"& dayOfMonth$Date<"2021-02-09 00:00:00",]



#Convert those dates!
#EasyTestDayofMonth$Day<-as.factor(EasyTestDayofMonth$Day)###This line you only need if your data column is not already a factor
#levels(EasyTestDayofMonth$Day)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
#EasyTestDayofMonth$Day<- factor("2","3","4","5","6","7","8",levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"))

#EasyTrainingDayofMonthIndicator<-class.ind(EasyTrainingDayofMonth$Day)
#EasyTestDayofMonthIndicator<-class.ind(EasyTestDayofMonth$Day)


#DiffTrainingDayofMonth <- dayOfMonth[dayOfMonth$Date>="2021-08-01 00:00:00"& dayOfMonth$Date<="2021-12-24 23:00:00",]
#DiffTestDayofMonth <- dayOfMonth[dayOfMonth$Date>="2021-12-25 00:00:00"&dayOfMonth$Date<="2021-12-31 23:00:00",]

#DiffTrainingDayofMonthIndicator<-class.ind(DiffTrainingDayofMonth$Day)
#DiffTestDayofMonthIndicator<-class.ind(DiffTestDayofMonth$Day)



#group all indicators together using only 6 days of week, 11 months, 23 hours, and 1 rain column
EasyTrainingAllIndicators<-data.frame(EasyTrainingDayIndicator[,2:7],EasyTrainingTimeIndicator[,1:23])
EasyTrainingAllIndicators <- EasyTrainingAllIndicators[c(-1,-3698),]
EasyTestAllIndicators<-data.frame(EasyTestDayIndicator[,1:6],EasyTestTimeIndicator[,1:23])
EasyTestAllIndicators <- EasyTestAllIndicators[c(-1,-170),]

DiffTrainingAllIndicators<-data.frame(DiffTrainingDayIndicator[,1:6],DiffTrainingTimeIndicator[,1:23])
DiffTrainingAllIndicators <- DiffTrainingAllIndicators[c(-1,-2),]
DiffTestAllIndicators<-data.frame(DiffTestDayIndicator[,1:6],DiffTestTimeIndicator[,1:23])
DiffTestAllIndicators <- DiffTestAllIndicators[c(-1,-2),]

#Train a model with Indicator Variables for Easy week

p<-24
d<-1
q<-24
P<-0
D<-0
Q<-0

sapply(EasyTrainingAllIndicators,function(x) sum(is.na(x)))
sapply(EasyTrainingTS,function(x) sum(is.na(x)))

#EasymodelIndicator <- auto.arima(EasyTrainingTS,xreg=data.matrix(EasyTrainingAllIndicators),seasonal=FALSE)
EasymodelIndicator <- Arima(EasyTrainingTS,xreg=data.matrix(EasyTrainingAllIndicators),order=c(p,d,q),seasonal=list(order=c(P,D,Q)))
summary(EasymodelIndicator)
EasyaicIndicatorModel<-EasymodelIndicator$aic
EasyresidualsIndicator <-as.vector(residuals(EasymodelIndicator))
EasyresidualsDFIndicator <- as.data.frame(EasyresidualsIndicator)


#Four in One Plots Indicator Model----
#get fitted values using the fitted() function  (from forecast library)
EasyfittedValuesIndicator<-as.vector(fitted(EasymodelIndicator))
EasyresidVsFittedIndicator <- data.frame(EasyresidualsIndicator,EasyfittedValuesIndicator)
EasyfittedDFIndicator<-data.frame(EasyTraining$Time,EasyfittedValuesIndicator)
names(EasyfittedDFIndicator)=c("Time","fittedValues")
EasyfittedDFIndicator$Time <- as.POSIXct(EasyfittedDFIndicator$Time,tz="",format = "%Y-%m-%d %H:%M:%S")

#plot ACF/PACF of residuals
EasygResidAcfIndicator=ggAcf(EasyresidualsIndicator,lag.max=50,main="ACF of the Residuals Indicator")
EasygResidPacfIndicator = ggPacf(EasyresidualsIndicator,lag.max=50,main="PACF of the Residuals Indicator")

grid.arrange(EasygResidAcfIndicator,EasygResidPacfIndicator,nrow=1)

##ggplot version of 4 in 1 plots
#Normal Probability Plot
EasygNormProbIndicator=ggplot(EasyresidualsDFIndicator,aes(sample=EasyresidualsIndicator))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
EasygFittedVsResidIndicator = ggplot(data=EasyresidVsFittedIndicator, aes(x=EasyfittedValuesIndicator,y=EasyresidualsIndicator))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
EasygResidHistIndicator = ggplot(EasyresidualsDFIndicator,aes(x=EasyresidualsIndicator))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
EasygResidVsOrderIndicator =ggplot(data=EasyresidualsDFIndicator, aes(x=rownames(EasyresidualsDFIndicator), y = EasyresidualsIndicator))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


fourIn1_Indicator = grid.arrange(EasygNormProbIndicator, EasygFittedVsResidIndicator,EasygResidHistIndicator, EasygResidVsOrderIndicator,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
EasygFittedVsActualsIndicator = ggplot(data=EasyfittedDFIndicator, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=EasyTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Month",y = "Sales in Thousands ($)")
EasygFittedVsActualsIndicator

#Forecast with Indicator Variables ----
EasyforecastIndicator<-forecast(EasymodelIndicator,xreg = data.matrix(EasyTestAllIndicators),h=168)

#match forecasts with dates
EasyforecastFrameInd<-data.frame(Time=EasyTest$Time,Forecast=as.numeric(EasyforecastIndicator$mean))

#Calculate MAPE
EasymapeIndicator=mean(abs(EasyforecastIndicator$mean - EasyTest$NetDemand)/EasyTest$NetDemand)*100
EasymapeIndicator = round(EasymapeIndicator, digits = 3)

#Lets plot our forecast, actuals, and confidence intervals for our validation set
EasyfittedDataIndicator<-data.frame(Time=EasyTraining$Time,NetDemand=EasyforecastIndicator$fitted)
EasyforecastValuesIndicator<-data.frame(Time=EasyTest$Time,NetDemand=EasyforecastIndicator$mean)
EasyforecastUpper95Indicator<-data.frame(Time=EasyTest$Time,NetDemand=EasyforecastIndicator$upper[,2])
EasyforecastLower95Indicator<-data.frame(Time=EasyTest$Time,NetDemand=EasyforecastIndicator$lower[,2])

#create a string for the graph title
titleString = paste(" Easy NetDemand Forecasts - INDICATOR")

#plot the forecast of the validation set
EasygValidationForecastIndicator <- ggplot(data = EasyTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =EasyfittedDataIndicator, aes(x=Time,y=NetDemand) )+                
  geom_line(data = EasyforecastValuesIndicator, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=EasyforecastUpper95Indicator, aes(x=Time, y=NetDemand),color="blue",linetype="longdash")+
  geom_line(data=EasyforecastLower95Indicator, aes(x=Time, y=NetDemand),color="blue",linetype="longdash")+
  labs(title = titleString, x = "Time",y = "Net Demand")
  #annotate(geom="text",x=EasyTraining$Time,y=4000,label=paste("MAPE=",EasymapeIndicator))
EasygValidationForecastIndicator

#zoom in on center of last year to see more detail
colors <- c("Actuals" = "black","95 CI"="blue","Forecast"= "red") ### Specify your colors
EasygValidationForecastZoomIndicator <- ggplot(data = EasyTest, aes(x=Time,y=NetDemand,color="Actuals"))+geom_point()+
  geom_line(data =EasyfittedDataIndicator, aes(x=Time,y=NetDemand) )+
  geom_line(data = EasyforecastValuesIndicator, aes(x=Time, y=NetDemand, color="Forecast"))+
  geom_line(data=EasyforecastUpper95Indicator, aes(x=Time, y=NetDemand,color="95 CI"),linetype="longdash")+
  geom_line(data=EasyforecastLower95Indicator, aes(x=Time, y=NetDemand,color="95 CI"),linetype="longdash")+
  labs(title = titleString, x = "Time",y = "Net Demand")+
  annotate(geom="text",x=EasyTest[30,1],y=5000,label=paste("MAPE=",EasymapeIndicator))+
  #annotate(geom="text",x=clothingValidation[2,1],y=16000,label=paste("MAPE=",mapeIndicator))+
  xlim(min(EasyTest$Time),max(EasyTest$Time))+
  scale_colour_manual(values= colors)+labs(colour="Legend")
EasygValidationForecastZoomIndicator

# INDICATOR SUMMARY #######
EasymodelIndicator$aic
EasymodelIndicator$aicc
EIndRMSE<- sqrt(mean((EasymodelIndicator$residuals)^2))
#EasymapeIndicator


#Easy model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("p","d","q","AIC Fit","RMSE Fit","MAPE Test")
EasymodelIndicatorResults = data.frame(matrix(ncol=6,nrow=0))
colnames(EasymodelIndicatorResults)=columnNames

#results = c(arimaorder(EasymodelIndicator)[1],arimaorder(EasymodelIndicator)[2],arimaorder(EasymodelIndicator)[3],
 #           round(EasymodelIndicator$aic,3),round(EIndRMSE,3),"Null")

results = c(p,d,q,round(EasymodelIndicator$aic,3),round(EIndRMSE,3),"Null")
EasymodelIndicatorResults[nrow(EasymodelIndicatorResults)+1,] = results
EasymodelIndicatorResults[6,6]<-EasymapeIndicator

#Train a model with Indicator Variables for Difficult week

p<-24
d<-1
q<-16
P<-0
D<-0
Q<-0

#DiffmodelIndicator <- auto.arima(DiffTrainingTS,xreg=data.matrix(DiffTrainingAllIndicators),seasonal=FALSE)
DiffmodelIndicator <- Arima(DiffTrainingTS,xreg=data.matrix(DiffTrainingAllIndicators),order=c(p,d,q),seasonal=list(order=c(P,D,Q)))
summary(DiffmodelIndicator)
DiffaicIndicatorModel<-DiffmodelIndicator$aicc
DiffresidualsIndicator <-as.vector(residuals(DiffmodelIndicator))
DiffresidualsDFIndicator <- as.data.frame(DiffresidualsIndicator)


#Four in One Plots Indicator Model----
#get fitted values using the fitted() function  (from forecast library)
DifffittedValuesIndicator<-as.vector(fitted(DiffmodelIndicator))
DiffresidVsFittedIndicator <- data.frame(DiffresidualsIndicator,DifffittedValuesIndicator)
DifffittedDFIndicator<-data.frame(DiffTraining$Time,DifffittedValuesIndicator)
names(DifffittedDFIndicator)=c("Time","fittedValues")
DifffittedDFIndicator$Time <- as.POSIXct(DifffittedDFIndicator$Time,tz="",format = "%Y-%m-%d %H:%M:%S")

#plot ACF/PACF of residuals
DiffgResidAcfIndicator=ggAcf(DiffresidualsIndicator,lag.max=50,main="ACF of the Residuals Indicator")
DiffgResidPacfIndicator = ggPacf(DiffresidualsIndicator,lag.max=50,main="PACF of the Residuals Indicator")

grid.arrange(DiffgResidAcfIndicator,DiffgResidPacfIndicator,nrow=1)

##ggplot version of 4 in 1 plots
#Normal Probability Plot
DiffgNormProbIndicator=ggplot(DiffresidualsDFIndicator,aes(sample=DiffresidualsIndicator))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
DiffgFittedVsResidIndicator = ggplot(data=DiffresidVsFittedIndicator, aes(x=DifffittedValuesIndicator,y=DiffresidualsIndicator))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
DiffgResidHistIndicator = ggplot(DiffresidualsDFIndicator,aes(x=DiffresidualsIndicator))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
DiffgResidVsOrderIndicator =ggplot(data=DiffresidualsDFIndicator, aes(x=rownames(DiffresidualsDFIndicator), y = DiffresidualsIndicator))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


fourIn1_Indicator = grid.arrange(DiffgNormProbIndicator, DiffgFittedVsResidIndicator,DiffgResidHistIndicator, DiffgResidVsOrderIndicator,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
DiffgFittedVsActualsIndicator = ggplot(data=DifffittedDFIndicator, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=DiffTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Month",y = "Sales in Thousands ($)")
DiffgFittedVsActualsIndicator

#Forecast with Indicator Variables ----
DiffforecastIndicator<-forecast(DiffmodelIndicator,xreg=data.matrix(DiffTestAllIndicators),h=168)

#match forecasts with dates
DiffforecastFrameInd<-data.frame(Time=DiffTest$Time,Forecast=as.numeric(DiffforecastIndicator$mean))

#Calculate MAPE
DiffmapeIndicator=mean(abs(DiffforecastIndicator$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DiffmapeIndicator = round(DiffmapeIndicator, digits = 3)

#Lets plot our forecast, actuals, and confidence intervals for our validation set
DifffittedDataIndicator<-data.frame(Time=DiffTraining$Time,NetDemand=DiffforecastIndicator$fitted)
DiffforecastValuesIndicator<-data.frame(Time=DiffTest$Time,NetDemand=DiffforecastIndicator$mean)
DiffforecastUpper95Indicator<-data.frame(Time=DiffTest$Time,NetDemand=DiffforecastIndicator$upper[,2])
DiffforecastLower95Indicator<-data.frame(Time=DiffTest$Time,NetDemand=DiffforecastIndicator$lower[,2])

#create a string for the graph title
titleString = paste(" Difficult NetDemand Forecasts - INDICATOR")

#plot the forecast of the validation set
DiffgValidationForecastIndicator <- ggplot(data = DiffTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =DifffittedDataIndicator, aes(x=Time,y=NetDemand) )+                
  geom_line(data = DiffforecastValuesIndicator, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=DiffforecastUpper95Indicator, aes(x=Time, y=NetDemand),color="blue",linetype="longdash")+
  geom_line(data=DiffforecastLower95Indicator, aes(x=Time, y=NetDemand),color="blue",linetype="longdash")+
  labs(title = titleString, x = "Time",y = "Net Demand")
#annotate(geom="text",x=EasyTraining$Time,y=4000,label=paste("MAPE=",EasymapeIndicator))
DiffgValidationForecastIndicator

#zoom in on center of last year to see more detail
colors <- c("Actuals" = "black","95 CI"="blue","Forecast"= "red") ### Specify your colors
DiffgValidationForecastZoomIndicator <- ggplot(data = DiffTest, aes(x=Time,y=NetDemand,color="Actuals"))+geom_point()+
  geom_line(data =DifffittedDataIndicator, aes(x=Time,y=NetDemand) )+
  geom_line(data = DiffforecastValuesIndicator, aes(x=Time, y=NetDemand, color="Forecast"))+
  geom_line(data=DiffforecastUpper95Indicator, aes(x=Time, y=NetDemand,color="95 CI"),linetype="longdash")+
  geom_line(data=DiffforecastLower95Indicator, aes(x=Time, y=NetDemand,color="95 CI"),linetype="longdash")+
  labs(title = titleString, x = "Time",y = "Net Demand")+
  annotate(geom="text",x=DiffTest[30,1],y=4500,label=paste("MAPE=",DiffmapeIndicator))+
  xlim(min(DiffTest$Time),max(DiffTest$Time))+
  scale_colour_manual(values= colors)+labs(colour="Legend")
DiffgValidationForecastZoomIndicator

# INDICATOR SUMMARY #######
DiffmodelIndicator$aic
DiffmodelIndicator$aicc
DIndRMSE<- sqrt(mean((DiffmodelIndicator$residuals)^2))
#EasymapeIndicator


#Difficult model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("p","d","q","AIC Fit","RMSE Fit","MAPE Test")
DiffmodelIndicatorResults = data.frame(matrix(ncol=6,nrow=0))
colnames(DiffmodelIndicatorResults)=columnNames

#results = c(arimaorder(DiffmodelIndicator)[1],arimaorder(DiffmodelIndicator)[2],arimaorder(DiffmodelIndicator)[3],
     #      round(DiffmodelIndicator$aic,3),round(DIndRMSE,3),"Null")

results = c(p,d,q,round(DiffmodelIndicator$aic,3),round(DIndRMSE,3),"Null")

DiffmodelIndicatorResults[nrow(DiffmodelIndicatorResults)+1,] = results

#DiffmodelIndicatorResults[6,6]<-DiffmapeIndicator
