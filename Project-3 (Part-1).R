# Project 3
# Team Member: Sonia, Saleh, Vineet

library(ggplot2)
library(gridExtra)
library(TSstudio)
library(zoo)
library(scales)
library(forecast)
library(plyr)
library(imputeTS)
library(corrplot)
library(nnet)

hrlyData <- read.table("hrlyData.csv",header=TRUE,sep=',')
hrlyData$Time <- as.POSIXct(hrlyData$Time,tz="",format = "%Y-%m-%d %H:%M:%S")
hrlyData <- hrlyData[,-1]
hrlyData[10515,1] <- "2021-03-14 01:00:00"
hrlyData[10515,1]

EasyTraining <- hrlyData[hrlyData$Time<"2021-02-02 00:00:00"& hrlyData$Time>="2020-09-01 00:00:00",c("Time","NetDemand")]
EasyTraining <- EasyTraining[c(-1,-3698),]

EasyTrainingTS <- ts(EasyTraining$NetDemand, frequency = 168)

EasyTest <- hrlyData[hrlyData$Time>="2021-02-02 00:00:00"& hrlyData$Time<"2021-02-09 00:00:00",c("Time","NetDemand")]
EasyTest <- EasyTest[c(-1,-170),]

plot(EasyTraining,main="Easy Training Set",type = "l", ylab="Net Demand",xlab="Time")
plot(EasyTest,main="Easy Test Set",type = "l", ylab="Net Demand",xlab="DTime")


DiffTraining <- hrlyData[hrlyData$Time>="2021-08-01 00:00:00"& hrlyData$Time<="2021-12-24 23:00:00",c("Time","NetDemand")]
DiffTraining <- DiffTraining[c(-1,-2),]

DiffTrainingTS <- ts(DiffTraining$NetDemand, frequency = 168)

DiffTest <- hrlyData[hrlyData$Time>="2021-12-25 00:00:00"&hrlyData$Time<="2021-12-31 23:00:00",c("Time","NetDemand")]
DiffTest <- DiffTest[c(-1,-2),]

plot(DiffTraining,main="Difficult Training Set",type = "l", ylab="Net Demand",xlab="Time")
plot(DiffTest,main="Difficult Test Set",type = "l", ylab="Net Demand",xlab="Time")

#plotting the acf and pacf for easy training set
Eacf=ggAcf(EasyTraining$NetDemand,lag.max=50,main="ACF of Easy Training Set", lwd=1.5)
Epacf=ggPacf(EasyTraining$NetDemand,lag.max=50,main="PACF of Easy Training Set", lwd=1.5)
grid.arrange(Eacf,Epacf,nrow=1)

#differencing the easy training set
EdiffData = diff(diff(EasyTraining$NetDemand,lag=1),lag=168)
EdiffDataWithMonths <- data.frame(EasyTraining$NetDemand[170:3696],EdiffData)
names(EdiffDataWithMonths) = c("Month","differences")

EacfDiff=ggAcf(EdiffData,lag.max=500,main="ACF of Differenced Easy Training Set", lwd=1.5)
EpacfDiff=ggPacf(EdiffData,lag.max=500,main="PACF of Differencec Easy Training Set", lwd=1.5)
grid.arrange(EacfDiff,EpacfDiff,nrow=1)


#plotting the acf and pacf for difficult training set
Dacf=ggAcf(DiffTraining$NetDemand,lag.max=50,main="ACF of difficult Training Set", lwd=1.5)
Dpacf=ggPacf(DiffTraining$NetDemand,lag.max=50,main="PACF of difficult Training Set", lwd=1.5)
grid.arrange(Dacf,Dpacf,nrow=1)

#differencing the difficult training set
DdiffData = diff(diff(DiffTraining$NetDemand,lag=1),lag=168)
DdiffDataWithMonths <- data.frame(DiffTraining$NetDemand[170:3504],DdiffData)
names(DdiffDataWithMonths) = c("Month","differences")

DacfDiff=ggAcf(DdiffData,lag.max=500,main="ACF of Differenced Difficult Training Set", lwd=1.5)
DpacfDiff=ggPacf(DdiffData,lag.max=500,main="PACF of Differenced Difficult Training Set", lwd=1.5)
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
# Model 1
p=2
d=1
q=1
P=0
D=1
Q=2

#modelArima <- auto.arima(EasyTrainingTS,seasonal=TRUE)
#modelArima
#Emodel <- arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="CSS")
Emodel <- Arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="ML")
Emodel


ERMSE <- sqrt(mean((Emodel$residuals)^2))
Eresults <- c(p,d,q,P,D,Q,Emodel$aic,ERMSE,NA)
EmodelParameters[nrow(EmodelParameters)+1,] = Eresults

#Model 2
p1=9
d1=1
q1=0
P1=0
D1=1
Q1=2

Emodel1 <- arima(EasyTraining$NetDemand,order=c(p1,d1,q1),seasonal=list(order=c(P1,D1,Q1),period=168),method="ML")
Emodel1

ERMSE1 <- sqrt(mean((Emodel1$residuals)^2))
Eresults1 <- c(p1,d1,q1,P1,D1,Q1,Emodel1$aic,ERMSE1,NA)
EmodelParameters[nrow(EmodelParameters)+1,] = Eresults1



## Residuals and Fitted Values

EResiduals <- as.vector(residuals(Emodel))
EResidualsDF <- as.data.frame(EResiduals)

EfittedValues<-as.vector(fitted(Emodel))
EresidVsFitted <- data.frame(EResiduals,EfittedValues)
EfittedDF<-data.frame(EasyTraining$Time,EfittedValues)
names(EfittedDF)=c("Time","fittedValues")

#plot ACF/PACF of residuals
gEResidAcf=ggAcf(EResiduals,lag.max=50,main="ACF of the Residuals for Easy Training Set")
gEResidPacf = ggPacf(EResiduals,lag.max=50,main="PACF of the Residuals for Easy Training Set")
grid.arrange(gEResidAcf,gEResidPacf,nrow=1)


## 4-in-1 Plots

#Normal Probability Plot
gENormProb=ggplot(EResidualsDF,aes(sample=EResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gEFittedVsResid = ggplot(data=EresidVsFitted, aes(x=EfittedValues,y=EResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gEResidHist = ggplot(EResidualsDF,aes(x=EResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEResidVsOrder =ggplot(data=EResidualsDF, aes(x=rownames(EResidualsDF), y = EResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


E4In1 = grid.arrange(gENormProb, gEFittedVsResid,gEResidHist, gEResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gEFittedVsActuals = ggplot(data=EfittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=EasyTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gEFittedVsActuals

# East Test Set Forecast (top 3 models)

EForecast <- forecast(Emodel,h=168)

MAPEArima <-mean(abs(EForecast$mean - EasyTest$NetDemand)/EasyTest$NetDemand)*100
MAPEArima = round(MAPEArima, digits = 3)  

EmodelParameters[3,9] <- MAPEArima

EfittedData <- data.frame(Time= EasyTraining$Time, NetDemand = EForecast$fitted)
EforecastValue <- data.frame(Time = EasyTest$Time, NetDemand = EForecast$mean)
EforecastUpper95 <- data.frame(Time = EasyTest$Time, Upper = EForecast$upper[,2])
EforecastLower95 <- data.frame(Time = EasyTest$Time, Lower = EForecast$lower[,2])

titleString1 = paste("Easy  Test Forecasts - ARIMA(",p,",",d,",",q,")X(",P,",",D,",",Q,")")

# Easy Test Set Forecast Plot

gETestForecast <- ggplot(data = EasyTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =EfittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = EforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=EforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=EforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString1, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=EasyTraining[500,1],y=7000,label=paste("MAPE=",MAPEArima))
gETestForecast


# Easy Test Set Zoomed Forecast

colors <- c("Actuals" = "black","95 CI"="blue","ARIMA Forecast"= "red")

EZoomedTEstForecast <- ggplot(EforecastValue,aes(x=Time,y=NetDemand, color="ARIMA Forecast"))+
  labs(y="Net Demand", x="Time",title="Forecast for the Easy Test Set- ARIMA")+
  geom_line(size = 1)+geom_point(data=EasyTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=EforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=EforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString1, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors)+labs(colour="Legend")+
  annotate(geom="text",x=EasyTest[50,1],y=7000,label=paste("MAPE=",MAPEArima))
EZoomedTEstForecast

# Difficult Test Set
#Model 1

p6=12
d6=1
q6=1
P6=1
D6=1
Q6=1

Dmodel <- arima(DiffTraining$NetDemand,order=c(p6,d6,q6),seasonal=list(order=c(P6,D6,Q6),period=168),method="ML")
Dmodel

DRMSE <- sqrt(mean((Dmodel$residuals)^2))
Dresults <- c(p6,d6,q6,P6,D6,Q6,Dmodel$aic,DRMSE,NA)
DmodelParameters[nrow(DmodelParameters)+1,] = Dresults


## Residuals and Fitted Values

DResiduals <- as.vector(residuals(Dmodel))
DResidualsDF <- as.data.frame(DResiduals)

DfittedValues<-as.vector(fitted(Dmodel))
DresidVsFitted <- data.frame(DResiduals,DfittedValues)
DfittedDF<-data.frame(DiffTraining$Time,DfittedValues)
names(DfittedDF)=c("Time","fittedValues")

#plot ACF/PACF of residuals
gDResidAcf=ggAcf(DResiduals,lag.max=50,main="ACF of the Residuals for Difficult Training Set")
gDResidPacf = ggPacf(DResiduals,lag.max=50,main="PACF of the Residuals for Difficult Training Set")
grid.arrange(gDResidAcf,gDResidPacf,nrow=1)


## 4-in-1 Plots

#Normal Probability Plot
gDNormProb=ggplot(DResidualsDF,aes(sample=DResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gDFittedVsResid = ggplot(data=DresidVsFitted, aes(x=DfittedValues,y=DResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gDResidHist = ggplot(DResidualsDF,aes(x=DResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gDResidVsOrder =ggplot(data=DResidualsDF, aes(x=rownames(DResidualsDF), y = DResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


D4In1 = grid.arrange(gDNormProb, gDFittedVsResid,gDResidHist, gDResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gDFittedVsActuals = ggplot(data=DfittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=DiffTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gDFittedVsActuals

# Difficult Test Set Forecast (top 3 models)

DForecast <- forecast(Dmodel,h=168)

DMAPEArima <-mean(abs(DForecast$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DMAPEArima = round(DMAPEArima, digits = 3)  

DmodelParameters[1,9] <- DMAPEArima

DfittedData <- data.frame(Time= DiffTraining$Time, NetDemand = DForecast$fitted)
DforecastValue <- data.frame(Time = DiffTest$Time, NetDemand = DForecast$mean)
DforecastUpper95 <- data.frame(Time = DiffTest$Time, Upper = DForecast$upper[,2])
DforecastLower95 <- data.frame(Time = DiffTest$Time, Lower = DForecast$lower[,2])

titleString9 = paste("Difficult  Test Forecasts - ARIMA(",p6,",",d6,",",q6,")X(",P6,",",D6,",",Q6,")")

# Difficult Test Set Forecast Plot

gDTestForecast <- ggplot(data = DiffTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =DfittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = DforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=DforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=DforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString9, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=DiffTraining[500,1],y=7000,label=paste("MAPE=",DMAPEArima))
gDTestForecast


# Difficult Test Set Zoomed Forecast

colors <- c("Actuals" = "black","95 CI"="blue","ARIMA Forecast"= "red")

DZoomedTEstForecast <- ggplot(DforecastValue,aes(x=Time,y=NetDemand, color="ARIMA Forecast"))+
  geom_line(size = 1)+geom_point(data=DiffTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=DforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=DforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString9, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors)+labs(colour="Legend")+
  annotate(geom="text",x=DiffTest[50,1],y=4000,label=paste("MAPE=",DMAPEArima))
DZoomedTEstForecast



## Part 4

RegressorVariable <- read.table(file="201806 202112 Raw Weather and RIT Calendar Data.csv",header=TRUE,sep=',')
RegressorVariable$Time <- as.POSIXct(RegressorVariable$Time,tz="",format = "%m/%d/%y %H:%M")

imputedExtTemp = na_ma(RegressorVariable$AW_ExtTemp,k=5,weighting="simple")
imputedExtHum = na_ma(RegressorVariable$AW_ExtHum,k=5,weighting="simple")
imputedExtDew = na_ma(RegressorVariable$AW_DewPoint,k=5,weighting="simple")
imputedFeelsLike = na_ma(RegressorVariable$AW_FeelsLike, k=5,weighting="simple")
imputedWindSpeed = na_locf(RegressorVariable$OA_WindSpe)

imputedRegressor <- data.frame(RegressorVariable$Time,imputedExtTemp, imputedExtHum, imputedExtDew, imputedFeelsLike, imputedWindSpeed)#created a data frame for Time and imputed moving average data values
colnames(imputedRegressor)<-c("Time","ExternalTemp","ExternalHumidity","ExternalDew","FeelsLike","WindSpeed")

#ggplot_na_distribution(imputedRegressor$ExternalTemp)

RegressorTime <- data.frame(hrlyData[5857:nrow(hrlyData),])


rollmeanTemp <- rollmean(imputedRegressor$ExternalTemp,12)
#ggplot_na_distribution(rollmeanTemp)

rollmeanHum <- rollmean(imputedRegressor$ExternalHumidity,12)
rollmeanDew <- rollmean(imputedRegressor$ExternalDew,12)
rollmeanWindSpeed <- rollmean(imputedRegressor$WindSpeed,12)
rollmeanFeels <- rollmean(imputedRegressor$FeelsLike,12)

rollmeanDF <- data.frame(Time=imputedRegressor$Time[12:nrow(imputedRegressor)],ExtTemp=rollmeanTemp, ExtHum= rollmeanHum,ExtDew = rollmeanDew, FeelsLike = rollmeanFeels, WindSpeed=rollmeanWindSpeed)
#ggplot_na_distribution(rollmeanDF$ExtTemp)


ClassInd <- data.frame(RegressorVariable$Ev_Classes)
ClassInd <- na_locf(ClassInd)
#BreakInd <- data.frame(RegressorVariable$Ev_SpringBreak)
#BreakInd <- na_locf(BreakInd)
#FirstDayInd <- data.frame(RegressorVariable$Ev_FirstDayAfterBreak)
#FirstDayInd<- na_locf(FirstDayInd)
#SemesterInd <- class.ind(RegressorVariable$Semester)
#SemesterInd <- na.locf(SemesterInd)

IndicatorVariable <- data.frame(Time = RegressorVariable$Time, Classes=ClassInd)

ABCD <- join(RegressorTime,RegressorVariable,by = "Time", type = "left", match = "first")
ABCD <- ABCD[,-c(5:nrow(ABCD))]
TimeoftheDayInd <- class.ind(ABCD$Time.1)
DayoftheWeedInd <- class.ind(ABCD$Day)

IndVariable <- data.frame(TimeoftheDay = TimeoftheDayInd[,1:23],DayoftheWeek = DayoftheWeedInd[,1:6])


FinalRegressorVariable <- join(RegressorTime,rollmeanDF,by = "Time", type = "left", match = "first")
#ggplot_na_distribution(FinalRegressorVariable$ExtTemp)
FinalRegressorVariable$WindSpeed <- round(FinalRegressorVariable$WindSpeed, digits = 4)

IndicatorData<- join(RegressorTime, IndicatorVariable, by = "Time", type = "left", match = "first")
#ggplot_na_distribution(IndicatorData$DayoftheWeek.Fri)

IndicatorData <- cbind(IndicatorData,IndVariable)

FinalRegressorVariable <- cbind(IndicatorData,FinalRegressorVariable)
FinalRegressorVariable <- FinalRegressorVariable[,c(-2,-33,-34)]


FinalTrainingData <- cbind(RegressorTime, FinalRegressorVariable)
FinalTrainingData <- FinalTrainingData[,-3]

#ggplot_na_distribution(FinalTrainingData$NetDemand)

frameForCor = FinalTrainingData[1500:3696,c(2,33:37)]
colnames(frameForCor)=c("NetDemand","ExtTemp","ExtHumidity","Dew","FeelsLike","WindSpeed")
corrplot(cor(frameForCor),method='ellipse',order="AOE",type="upper")


## Regerssor Arima Model  
#Model 1

RegTrainingVariable <-FinalRegressorVariable[1:3696,c(2:33,36)] 
#ggplot_na_distribution(RegTrainingVariable$WindSpeed)


RegcolumnNames = c("Dew","Feels Like","Ext Temp","Wind Speed","Ext Humidity","Class","p","d","q","AIC Fit","RMSE")
RegressorParameters = data.frame(matrix(ncol=11,nrow=0))
colnames(RegressorParameters)=RegcolumnNames

p2=15
d2=1
q2=15
P2=0
D2=0
Q2=0

RegressorArima <- arima(EasyTraining$NetDemand,xreg=data.matrix(RegTrainingVariable),order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period =168),)
#RegressorArima <- auto.arima(EasyTrainingTS,xreg=data.matrix(RegTrainingVariable),seasonal=FALSE)
RegressorArima

RegRMSE <- sqrt(mean((RegressorArima$residuals)^2))
RegRMSE <- round(RegRMSE, digit =3)

RegResults <- c("N","N","Y","Y","Y","Y",p2,d2,q2,RegressorArima$aic, RegRMSE)
RegressorParameters[nrow(RegressorParameters)+1,] = RegResults

#Model 2

#RegTrainingVariable1 <-FinalRegressorVariable[1:3696,c(2:33,36)] 
#ggplot_na_distribution(RegTrainingVariable1$TimeoftheDay.0.00.00)
#EasyTrainingTS1 <- ts(EasyTraining$NetDemand, frequency = 168)


#p5=15
#d5=1
#q5=15
#P5=0
#D5=0
#Q5=0

#RegressorArima1 <- arima(EasyTraining$NetDemand,xreg=data.matrix(RegTrainingVariable1),order=c(p5,d5,q5),seasonal=list(order=c(P5,D5,Q5),period =168),)
#RegressorArima1 <- auto.arima(EasyTrainingTS1,xreg=data.matrix(RegTrainingVariable1),seasonal=FALSE)
#RegressorArima1

#RegRMSE1 <- sqrt(mean((RegressorArima1$residuals)^2))
#RegRMSE1 <- round(RegRMSE1, digit =3)


## Residuals and Fit for the best model parameters.

RegressorResiduals <- as.vector(residuals(RegressorArima))
RegressorResidualsDF <- as.data.frame(RegressorResiduals)

RegressorfittedValues<-as.vector(fitted(RegressorArima))
RegressorresidVsFitted <- data.frame(RegressorResiduals,RegressorfittedValues)
RegressorfittedDF<-data.frame(EasyTraining$Time,RegressorfittedValues)
names(RegressorfittedDF)=c("Time","fittedValues")


#Normal Probability Plot
gRegressorNormProb=ggplot(RegressorResidualsDF,aes(sample=RegressorResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gRegressorFittedVsResid = ggplot(data=RegressorresidVsFitted, aes(x=RegressorfittedValues,y=RegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gRegressorResidHist = ggplot(RegressorResidualsDF,aes(x=RegressorResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gRegressorResidVsOrder =ggplot(data=RegressorResidualsDF, aes(x=rownames(RegressorResidualsDF), y = RegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


ERegressor4In1 = grid.arrange(gRegressorNormProb, gRegressorFittedVsResid,gRegressorResidHist, gRegressorResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gRegressorFittedVsActuals = ggplot(data=RegressorfittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=EasyTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gRegressorFittedVsActuals

# Part 5
CrossCorrelation1 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$ExtTemp[1:3696],lag.max = 50)
CrossCorrelation2 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$ExtHum[1:3696],lag.max = 50)
CrossCorrelation3 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$ExtDew[1:3696],lag.max = 500)
CrossCorrelation4 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$FeelsLike[1:3696],lag.max = 500)
CrossCorrelation5 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$WindSpeed[1:3696],lag.max = 50)
CrossCorrelation5 <- ccf(FinalTrainingData$NetDemand[1:3696],FinalTrainingData$Classes[1:3696],lag.max = 500)


TempLag1 <- lag(FinalRegressorVariable[,32],-840)
HumLag1 <- lag(FinalRegressorVariable[,33],-840)
DewLag1 <- lag(FinalRegressorVariable[,34],-840)
FeelsLag1 <- lag(FinalRegressorVariable[,35],-840)
WindLag1 <- lag(FinalRegressorVariable[,36],-840)


LaggedRegressorVariable <- data.frame(LagTemp=TempLag1, LagHum = HumLag1, LagDew=DewLag1, LagFeels=FeelsLag1,LagWind=WindLag1)
FinalLaggedRegVariables <- cbind(FinalRegressorVariable[,2:31],LaggedRegressorVariable)

LagRegcolumnNames = c("Lag","Dew","Feels Like","Ext Temp","Wind Speed","Ext Humidity","Class","p","d","q","AIC Fit","RMSE")
LagRegressorParameters = data.frame(matrix(ncol=12,nrow=0))
colnames(LagRegressorParameters)=LagRegcolumnNames

p3=15
d3=1
q3=15
P3=0
D3=0
Q3=0

LaggedRegressorArima <- arima(EasyTraining$NetDemand,xreg=data.matrix(FinalLaggedRegVariables[1:3696,c(1:32,35)]),order=c(p3,d3,q3),seasonal=list(order=c(P3,D3,Q3),period=168),)
LaggedRegressorArima


LaggedRegRMSE <- sqrt(mean((LaggedRegressorArima$residuals)^2))
LaggedRegRMSE <- round(LaggedRegRMSE, digit =3)

LagRegResults <- c("840","N","N","Y","Y","Y","Y",p3,d3,q3,LaggedRegressorArima$aic, LaggedRegRMSE)
LagRegressorParameters[nrow(LagRegressorParameters)+1,] = LagRegResults


# Part 6

#For Easy Week

RegTrainingVariable <-FinalRegressorVariable[1:3696,c(2:33,36)] 
RegTestVariable <- FinalRegressorVariable[3697:3864,c(2:33,36)]

p2=15
d2=1
q2=15
P2=0
D2=0
Q2=0

RegressorArima <- Arima(EasyTraining$NetDemand,xreg=data.matrix(RegTrainingVariable),order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period=168),)
RegressorArima

# Easy Week Forecast

EasyRegForecast <- forecast(RegressorArima,xreg=data.matrix(RegTestVariable),h=168)

EasyRegMAPE <-mean(abs(EasyRegForecast$mean - EasyTest$NetDemand)/EasyTest$NetDemand)*100
EasyRegMAPE = round(EasyRegMAPE, digits = 3)  

ERegfittedData <- data.frame(Time= EasyTraining$Time, NetDemand = EasyRegForecast$fitted)
ERegforecastValue <- data.frame(Time = EasyTest$Time, NetDemand = EasyRegForecast$mean)
ERegforecastUpper95 <- data.frame(Time = EasyTest$Time, Upper = EasyRegForecast$upper[,2])
ERegforecastLower95 <- data.frame(Time = EasyTest$Time, Lower = EasyRegForecast$lower[,2])

titleString2 = paste("Easy Test Forecasts - Dynamic Regression ARIMA(",p2,",",d2,",",q2,")")

# Easy Test Set Forecast Plot

gEasyRegTestForecast <- ggplot(data = EasyTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =ERegfittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = ERegforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=ERegforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=ERegforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString2, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=EasyTraining[500,1],y=4500,label=paste("MAPE=",EasyRegMAPE))
gEasyRegTestForecast


# Easy Test Set Zoomed Forecast

colors1 <- c("Actuals" = "black","95 CI"="blue","Dynamic Regressor Forecast"= "red")

EasyRegZoomedTestForecast <- ggplot(ERegforecastValue,aes(x=Time,y=NetDemand, color="Dynamic Regressor Forecast"))+
  labs(y="Net Demand", x="Time",title="Forecast for the Easy Test Set- Dynamic Regression")+
  geom_line(size = 1)+geom_point(data=EasyTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=ERegforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=ERegforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString2, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors1)+labs(colour="Legend")+
  annotate(geom="text",x=EasyTest[25,1],y=4700,label=paste("MAPE=",EasyRegMAPE))
EasyRegZoomedTestForecast

# Residuals for forecast

EforecastResidualsReg <- as.vector(residuals(EasyRegForecast))
EforecastResidualsDFReg <- as.data.frame(EforecastResidualsReg) 

EforecastfittedReg<-as.vector(fitted(EasyRegForecast))
EForecastResidVsFittedReg <- data.frame(EforecastResidualsReg,EforecastfittedReg)

gEForNormProbReg=ggplot(EforecastResidualsDFReg,aes(sample=EforecastResidualsReg))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Normal Probability Plot")

#Fitted Vs Residuals
gEForFittedVsResidReg = ggplot(data=EForecastResidVsFittedReg, aes(x=EforecastfittedReg,y=EforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gEForResidHistReg = ggplot(EforecastResidualsDFReg,aes(x=EforecastResidualsReg))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Forecast- Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEForResidVsOrderReg =ggplot(data=EforecastResidualsDFReg, aes(x=rownames(EforecastResidualsDFReg), y = EforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Forecast- Residuals vs Observation Order", x = "Order",y = "Residuals")


EForfourIn1_Reg = grid.arrange(gEForNormProbReg, gEForFittedVsResidReg,gEForResidHistReg, gEForResidVsOrderReg,nrow=2,ncol=2)


# Comparision Between Easy ARIMA and Easy Dynamic Regression

grid.arrange(EZoomedTEstForecast,EasyRegZoomedTestForecast, nrow=2)

# For difficult week

DiffRegTrainingVariable <-FinalRegressorVariable[8017:11520,c(2:33,36)] 
DiffRegTestVariable <- FinalRegressorVariable[11521:nrow(FinalRegressorVariable),c(2:33,36)]

p2=15
d2=1
q2=15
P2=0
D2=0
Q2=0

DiffRegressorArima <- Arima(DiffTraining$NetDemand,xreg=data.matrix(DiffRegTrainingVariable),order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period=168),)
DiffRegressorArima

## Residuals and Fit for the best model parameters.

DiffRegressorResiduals <- as.vector(residuals(DiffRegressorArima))
DiffRegressorResidualsDF <- as.data.frame(DiffRegressorResiduals)

DiffRegressorfittedValues<-as.vector(fitted(DiffRegressorArima))
DiffRegressorresidVsFitted <- data.frame(DiffRegressorResiduals,DiffRegressorfittedValues)
DiffRegressorfittedDF<-data.frame(DiffTraining$Time,DiffRegressorfittedValues)
names(DiffRegressorfittedDF)=c("Time","fittedValues")


#Normal Probability Plot
gDiffRegressorNormProb=ggplot(DiffRegressorResidualsDF,aes(sample=DiffRegressorResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gDiffRegressorFittedVsResid = ggplot(data=DiffRegressorresidVsFitted, aes(x=DiffRegressorfittedValues,y=DiffRegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gDiffRegressorResidHist = ggplot(DiffRegressorResidualsDF,aes(x=DiffRegressorResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gDiffRegressorResidVsOrder =ggplot(data=DiffRegressorResidualsDF, aes(x=rownames(DiffRegressorResidualsDF), y = DiffRegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


EDiffRegressor4In1 = grid.arrange(gDiffRegressorNormProb, gDiffRegressorFittedVsResid,gDiffRegressorResidHist, gDiffRegressorResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gDiffRegressorFittedVsActuals = ggplot(data=DiffRegressorfittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=DiffTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gDiffRegressorFittedVsActuals

# Difficult Week Forecast

DiffRegForecast <- forecast(DiffRegressorArima,xreg=data.matrix(DiffRegTestVariable),h=168)

DiffRegMAPE <-mean(abs(DiffRegForecast$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DiffRegMAPE = round(DiffRegMAPE, digits = 3)  

DRegfittedData <- data.frame(Time= DiffTraining$Time, NetDemand = DiffRegForecast$fitted)
DRegforecastValue <- data.frame(Time = DiffTest$Time, NetDemand = DiffRegForecast$mean)
DRegforecastUpper95 <- data.frame(Time = DiffTest$Time, Upper = DiffRegForecast$upper[,2])
DRegforecastLower95 <- data.frame(Time = DiffTest$Time, Lower = DiffRegForecast$lower[,2])

titleString3 = paste("Difficult Test Forecasts - Dynamic Regression ARIMA(",p2,",",d2,",",q2,")")

# Difficult Test Set Forecast Plot

gDiffRegTestForecast <- ggplot(data = DiffTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =DRegfittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = DRegforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=DRegforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=DRegforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString3, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=DiffTraining[2500,1],y=6000,label=paste("MAPE=",DiffRegMAPE))
gDiffRegTestForecast


# Difficult Test Set Zoomed Forecast


DiffRegZoomedTestForecast <- ggplot(DRegforecastValue,aes(x=Time,y=NetDemand, color="Dynamic Regressor Forecast"))+
  geom_line(size = 1)+geom_point(data=DiffTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=DRegforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=DRegforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString3, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors1)+labs(colour="Legend")+
  labs(title = titleString3, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=DiffTest[50,1],y=4000,label=paste("MAPE=",DiffRegMAPE))
DiffRegZoomedTestForecast

# Residuals for forecast

DforecastResidualsReg <- as.vector(residuals(DiffRegForecast))
DforecastResidualsDFReg <- as.data.frame(DforecastResidualsReg) 

DforecastfittedReg<-as.vector(fitted(DiffRegForecast))
DForecastResidVsFittedReg <- data.frame(DforecastResidualsReg,DforecastfittedReg)

gDForNormProbReg=ggplot(DforecastResidualsDFReg,aes(sample=DforecastResidualsReg))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Normal Probability Plot")

#Fitted Vs Residuals
gDForFittedVsResidReg = ggplot(data=DForecastResidVsFittedReg, aes(x=DforecastfittedReg,y=DforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gDForResidHistReg = ggplot(DforecastResidualsDFReg,aes(x=DforecastResidualsReg))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Forecast- Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gDForResidVsOrderReg =ggplot(data=DforecastResidualsDFReg, aes(x=rownames(DforecastResidualsDFReg), y = DforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Forecast- Residuals vs Observation Order", x = "Order",y = "Residuals")


DForfourIn1_Reg = grid.arrange(gDForNormProbReg, gDForFittedVsResidReg,gDForResidHistReg, gDForResidVsOrderReg,nrow=2,ncol=2)


# Comparision Between Easy ARIMA and Easy Dynamic Regression

grid.arrange(DZoomedTEstForecast,DiffRegZoomedTestForecast, nrow=2)

# Showcase Week

ShowcaseTraining <- FinalTrainingData[2929:6552,]
ShowcaseTrainingTS <- ts(ShowcaseTraining$NetDemand, frequency = 168)
ShowcaseTest <- FinalTrainingData[6553:6720,]

ShowcaseTrainingRegressor <- ShowcaseTraining[,c(3:34,37)]
ShowcaseTestRegressor <- ShowcaseTest[,c(3:34,37)]


p2=15
d2=1
q2=15
P2=0
D2=0
Q2=0

ShowRegressorArima <- Arima(ShowcaseTraining$NetDemand,xreg=data.matrix(ShowcaseTrainingRegressor),order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period=168),)
ShowRegressorArima

## Residuals and Fit for the best model parameters.

ShowRegressorResiduals <- as.vector(residuals(ShowRegressorArima))
ShowRegressorResidualsDF <- as.data.frame(ShowRegressorResiduals)

ShowRegressorfittedValues<-as.vector(fitted(ShowRegressorArima))
ShowRegressorresidVsFitted <- data.frame(ShowRegressorResiduals,ShowRegressorfittedValues)
ShowRegressorfittedDF<-data.frame(ShowcaseTraining$Time,ShowRegressorfittedValues)
names(ShowRegressorfittedDF)=c("Time","fittedValues")


#Normal Probability Plot
gShowRegressorNormProb=ggplot(ShowRegressorResidualsDF,aes(sample=ShowRegressorResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gShowRegressorFittedVsResid = ggplot(data=ShowRegressorresidVsFitted, aes(x=ShowRegressorfittedValues,y=ShowRegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gShowRegressorResidHist = ggplot(ShowRegressorResidualsDF,aes(x=ShowRegressorResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gShowRegressorResidVsOrder =ggplot(data=ShowRegressorResidualsDF, aes(x=rownames(ShowRegressorResidualsDF), y = ShowRegressorResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


EShowRegressor4In1 = grid.arrange(gShowRegressorNormProb, gShowRegressorFittedVsResid,gShowRegressorResidHist, gShowRegressorResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gShowRegressorFittedVsActuals = ggplot(data=ShowRegressorfittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=ShowcaseTraining, aes(x=Time,y=NetDemand),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gShowRegressorFittedVsActuals

# Forecast for the showcase period

ShowRegressionForecast <- forecast(ShowRegressorArima,xreg=data.matrix(ShowcaseTestRegressor),h=168)

ShowRegMAPE <-mean(abs(ShowRegressionForecast$mean - ShowcaseTest$NetDemand)/ShowcaseTest$NetDemand)*100
ShowRegMAPE = round(ShowRegMAPE, digits = 3)  

ShowRegfittedData <- data.frame(Time= ShowcaseTraining$Time, NetDemand = ShowRegressionForecast$fitted)
ShowRegforecastValue <- data.frame(Time = ShowcaseTest$Time, NetDemand = ShowRegressionForecast$mean)
ShowRegforecastUpper95 <- data.frame(Time = ShowcaseTest$Time, Upper = ShowRegressionForecast$upper[,2])
ShowRegforecastLower95 <- data.frame(Time = ShowcaseTest$Time, Lower = ShowRegressionForecast$lower[,2])

titleString4 = paste("Showcase Test Forecasts - Dynamic Regression (",p2,",",d2,",",q2,")")

# Showcase Test Set Forecast Plot

gShowRegTestForecast <- ggplot(data = ShowcaseTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =ShowRegfittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = ShowRegforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=ShowRegforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=ShowRegforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString4, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=ShowcaseTraining[1000,1],y=4500,label=paste("MAPE=",ShowRegMAPE))
gShowRegTestForecast


# Showcase Test Set Zoomed Forecast

colors1 <- c("Actuals" = "black","95 CI"="blue","Dynamic Regression Forecast"= "red")

ShowRegZoomedTestForecast <- ggplot(ShowRegforecastValue,aes(x=Time,y=NetDemand, color="Dynamic Regression Forecast"))+
  labs(y="Net Demand", x="Time",title="Dynamic Regression Forecast for the Showcase Test Set")+
  geom_line(size = 1)+geom_point(data=ShowcaseTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=ShowRegforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=ShowRegforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString4, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors1)+labs(colour="Legend")+
  annotate(geom="text",x=ShowcaseTest[50,1],y=4000,label=paste("MAPE=",ShowRegMAPE))
ShowRegZoomedTestForecast

# Residuals for forecast

ShowforecastResidualsReg <- as.vector(residuals(ShowRegressionForecast))
ShowforecastResidualsDFReg <- as.data.frame(ShowforecastResidualsReg) 

ShowforecastfittedReg<-as.vector(fitted(ShowRegressionForecast))
ShowForecastResidVsFittedReg <- data.frame(ShowforecastResidualsReg,ShowforecastfittedReg)

gShowForNormProbReg=ggplot(ShowforecastResidualsDFReg,aes(sample=ShowforecastResidualsReg))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Normal Probability Plot")

#Fitted Vs Residuals
gShowForFittedVsResidReg = ggplot(data=ShowForecastResidVsFittedReg, aes(x=ShowforecastfittedReg,y=ShowforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Forecast- Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gShowForResidHistReg = ggplot(ShowforecastResidualsDFReg,aes(x=ShowforecastResidualsReg))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Forecast- Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gShowForResidVsOrderReg =ggplot(data=ShowforecastResidualsDFReg, aes(x=rownames(ShowforecastResidualsDFReg), y = ShowforecastResidualsReg))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Forecast- Residuals vs Observation Order", x = "Order",y = "Residuals")


ShowForfourIn1_Reg = grid.arrange(gShowForNormProbReg, gShowForFittedVsResidReg,gShowForResidHistReg, gShowForResidVsOrderReg,nrow=2,ncol=2)


# Winter's method to Showcase test Set
ShowHW <- HoltWinters(ShowcaseTrainingTS,alpha=0.05,beta=0,gamma=1,seasonal = "multiplicative")

ShowHWForecast <- predict(ShowHW,n.ahead=168,prediction.interval=TRUE)

ShowHWPrediction <- ts(ShowHWForecast[,1])
ShowHWForecastDF <- data.frame(Time=ShowcaseTest$Time,ForecastValue=ShowHWPrediction)
ShowHWForecastDF$ForecastValue <- as.numeric(ShowHWForecastDF$ForecastValue)

ShowHWupperCI = ts(ShowHWForecast[,2])
ShowHWUpperCI  = data.frame(Time=ShowcaseTest$Time,UpperCI = ShowHWupperCI)
ShowHWlowerCI = ts(ShowHWForecast[,3])
ShowHWLowerCI =  data.frame(Time=ShowcaseTest$Time, LowerCI=ShowHWlowerCI)

ShowHWMAPE <-mean(abs(ShowHWForecastDF$ForecastValue - ShowcaseTest$NetDemand)/ShowcaseTest$NetDemand)*100
ShowHWMAPE = round(ShowHWMAPE, digits = 3)

titleString5 = paste("Showcase Test Forecasts - Winter's Method (",alpha,",",beta,",",gamma,")")

colors2 <- c("Actual" = "black","Winter's Forecast"="Red","95% CI"="green")
ShowHWForecastPlot<- ggplot(ShowHWForecastDF,aes(x=Time,y=ForecastValue, color="Winter's Forecast"))+
  labs(y="Net Demand", x="Time",title="Winter's Forecast for the Show Case Test Set")+
  geom_line(size = 1)+geom_point(data=ShowcaseTest,aes(x=Time,y=NetDemand, color="Actual"))+
  geom_line(data=ShowHWUpperCI,aes(x=Time,y=UpperCI, color="95% CI"))+
  geom_line(data=ShowHWLowerCI,aes(x=Time,y=LowerCI, color="95% CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  scale_colour_manual(values= colors2)+labs(colour="Legend")+
  annotate(geom="text",x=ShowcaseTest[50,1],y=4000,label=paste("MAPE=",ShowHWMAPE))+
  labs(title = titleString4, x = "Time",y = "Net Demand (Kw)")+
ShowHWForecastPlot

# ARIMA to Showcase

ShowACF <- ggAcf(ShowcaseTraining$NetDemand,lag.max=50,main="ACF of Showcase Training Set", lwd=1.5)
ShowPACF <- ggPacf(ShowcaseTraining$NetDemand,lag.max=50,main="PACF of Showcase Training Set", lwd=1.5)
grid.arrange(ShowACF, ShowPACF, nrow=2)

ShowDiffData = diff(diff(ShowcaseTraining$NetDemand,lag=1),lag=168)
ShowdiffDataWithMonths <- data.frame(ShowcaseTraining$NetDemand[170:3624],ShowDiffData)
names(ShowdiffDataWithMonths) = c("Month","differences")

ShowacfDiff=ggAcf(ShowDiffData,lag.max=500,main="ACF of differenced Showcase Training Set", lwd=1.5)
ShowpacfDiff=ggPacf(ShowDiffData,lag.max=500,main="PACF of differenced Showcase Training Set", lwd=1.5)
grid.arrange(ShowacfDiff,ShowpacfDiff,nrow=1)

#Model 1
p3=9
d3=1
q3=2
P3=1
D3=1
Q3=1

ShowArima <- arima(ShowcaseTraining$NetDemand,order=c(p3,d3,q3),seasonal=list(order=c(P3,D3,Q3),period=168),)
ShowArima

# Model 2
p4=1
d4=1
q4=1
P4=1
D4=1
Q4=1

ShowArima1 <- arima(ShowcaseTraining$NetDemand,order=c(p4,d4,q4),seasonal=list(order=c(P4,D4,Q4),period=168),)
ShowArima1

# Forecast
ShowArimaForecast <- forecast(ShowArima,h=168)

ShowArimaMAPE <-mean(abs(ShowArimaForecast$mean - ShowcaseTest$NetDemand)/ShowcaseTest$NetDemand)*100
ShowArimaMAPE = round(ShowArimaMAPE, digits = 3)  


ShowArimafittedData <- data.frame(Time= ShowcaseTraining$Time, NetDemand = ShowArimaForecast$fitted)
ShowArimaforecastValue <- data.frame(Time = ShowcaseTest$Time, NetDemand = ShowArimaForecast$mean)
ShowArimaforecastUpper95 <- data.frame(Time = ShowcaseTest$Time, Upper = ShowArimaForecast$upper[,2])
ShowArimaforecastLower95 <- data.frame(Time = ShowcaseTest$Time, Lower = ShowArimaForecast$lower[,2])

titleString6 = paste("Showcase Test Forecasts - ARIMA(",p3,",",d3,",",q3,")X(",P3,",",D3,",",Q3,")")

# Easy Test Set Forecast Plot

gShowArimaForecast <- ggplot(data = ShowcaseTraining, aes(x=Time,y=NetDemand))+geom_point()+
  geom_line(data =ShowArimafittedData, aes(x=Time,y=NetDemand) )+                
  geom_line(data = ShowArimaforecastValue, aes(x=Time, y=NetDemand), color="red")+
  geom_line(data=ShowArimaforecastUpper95, aes(x=Time, y=Upper),color="blue",linetype="longdash")+
  geom_line(data=ShowArimaforecastLower95, aes(x=Time, y=Lower),color="blue",linetype="longdash")+
  labs(title = titleString6, x = "Time",y = "Net Demand (Kw)")+
  annotate(geom="text",x=ShowcaseTraining[500,1],y=7000,label=paste("MAPE=",ShowArimaMAPE))
gShowArimaForecast


# Easy Test Set Zoomed Forecast

colors3 <- c("Actuals" = "black","95 CI"="blue","ARIMA Forecast"= "red")

ZoomedShowArimaForecast <- ggplot(ShowArimaforecastValue,aes(x=Time,y=NetDemand, color="ARIMA Forecast"))+
  labs(y="Net Demand", x="Time",title="ARIMA Forecast for the Showcase Test Set")+
  geom_line(size = 1)+geom_point(data=ShowcaseTest,aes(x=Time,y=NetDemand, color="Actuals"))+
  geom_line(data=ShowArimaforecastUpper95,aes(x=Time,y=Upper, color="95 CI"))+
  geom_line(data=ShowArimaforecastLower95,aes(x=Time,y=Lower, color="95 CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString6, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors3)+labs(colour="Legend")+
  annotate(geom="text",x=ShowcaseTest[50,1],y=4500,label=paste("MAPE=",ShowArimaMAPE))
ZoomedShowArimaForecast

grid.arrange(ShowRegZoomedTestForecast,ShowHWForecastPlot,ZoomedShowArimaForecast, nrow=3)

## Fourier Moddel

#for easy week
columnFourier <- c("Best i","Model AIC","RMSE", "Test MAPE")
FourierParameters <- data.frame(matrix(ncol=4,nrow=0))
colnames(FourierParameters) <- columnFourier

#Determine Best Number of Seasonal Patterns #########################################

#create variables to store the best model and best i values
aics <-data.frame(i=numeric(), aicc=numeric())
bestfit <- list(aicc=Inf)
besti<-0

for(i in 20:25) {
  fourierTransform<-fourier(EasyTrainingTS, K=c(i))#create the fourier transforms using i transforms for monthly
  fit<-auto.arima(EasyTrainingTS,xreg=fourierTransform,seasonal=FALSE) #Could also try seasonal=TRUE
  results<-data.frame(i,fit$aicc)
  aics<-rbind(aics,results)
  if(fit$aicc < bestfit$aicc){
    bestfit <- fit
    besti<-i
    
  }
}

#Forecast Using Your Best Fourier Transforms####
#create fourier transform for best parameters
fourierBest<-fourier(EasyTrainingTS, K=c(besti))
modelFourier <- auto.arima(EasyTrainingTS,xreg=fourierBest,seasonal=FALSE)

FourierRSME <- sqrt(mean((modelFourier$residuals)^2))

resultsFourier <- c(besti,modelFourier$aic,FourierRSME,NA)
FourierParameters[nrow(FourierParameters)+1,] = resultsFourier


## Residuals and Fitted Values

EFourResiduals <- as.vector(residuals(modelFourier))
EFourResidualsDF <- as.data.frame(EFourResiduals)

EFourfittedValues<-as.vector(fitted(modelFourier))
EFourresidVsFitted <- data.frame(EFourResiduals,EFourfittedValues)
EFourfittedDF<-data.frame(EasyTraining$Time,EFourfittedValues)
names(EFourfittedDF)=c("Time","fittedValues")

## 4-in-1 Plots

#Normal Probability Plot
gEFourNormProb=ggplot(EFourResidualsDF,aes(sample=EFourResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gEFourFittedVsResid = ggplot(data=EFourresidVsFitted, aes(x=EFourfittedValues,y=EFourResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gEFourResidHist = ggplot(EFourResidualsDF,aes(x=EFourResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEFourResidVsOrder =ggplot(data=EFourResidualsDF, aes(x=rownames(EFourResidualsDF), y = EFourResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


EFour4In1 = grid.arrange(gEFourNormProb, gEFourFittedVsResid,gEFourResidHist, gEFourResidVsOrder,nrow=2,ncol=2)


# Forecast 
fourierFuture<-fourier(EasyTrainingTS, K=c(besti),h=168)#create the future fourier values
forecastFourier<-forecast(modelFourier,xreg=fourierFuture,h=168)

#match forecasts with dates
forecastFrameFourier<-data.frame(Time=EasyTest$Time,Forecast=as.numeric(forecastFourier$mean))

mapeFourier=mean(abs(forecastFourier$mean - EasyTest$NetDemand)/EasyTest$NetDemand)*100
mapeFourier = round(mapeFourier, digits = 3)

FourierParameters[1,4] <- mapeFourier

#Make fancy plots
upperCIFourierdf = data.frame(Time=EasyTest$Time,upperCI=as.numeric(forecastFourier$upper))
upperCIFourierdf<- upperCIFourierdf[-c(169:nrow(upperCIFourierdf)),]
lowerCIFourierdf = data.frame(Time=EasyTest$Time,lowerCI=as.numeric(forecastFourier$lower))
lowerCIFourierdf<- lowerCIFourierdf[-c(169:nrow(lowerCIFourierdf)),]

titleString7 = paste("Easy Test Set Forecasts - Fourier (Best i =",besti,")")

colors4 <- c("Fourier Forecast" = "red","Actuals"="black", "95% CI"="blue")  #define colors for the graph to be used later
gTrainAndForecastFourier<-ggplot(EasyTest, aes(x=Time,y=NetDemand,colour="Actuals"))+
  geom_point()+
  geom_line(data=forecastFrameFourier,aes(x=Time,y=Forecast,colour="Fourier Forecast"))+
  geom_line(data=upperCIFourierdf,aes(x=Time,y=upperCI,colour="95% CI"))+
  geom_line(data=lowerCIFourierdf,aes(x=Time,y=lowerCI,colour="95% CI"))+
  geom_point(size = 1)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  annotate(geom="text",x=EasyTest[2,1],y=4500,label=paste("MAPE=",mapeFourier))+
  labs(title = titleString7, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors4) +labs(colour="Legend")
gTrainAndForecastFourier

#for difficult week
columnFourier <- c("Best i","Model AIC","RMSE", "Test MAPE")
DFourierParameters <- data.frame(matrix(ncol=4,nrow=0))
colnames(DFourierParameters) <- columnFourier

#Determine Best Number of Seasonal Patterns #########################################

#create variables to store the best model and best i values
Daics <-data.frame(j=numeric(), aicc=numeric())
Dbestfit <- list(aicc=Inf)
bestj<-0

for(j in 20:25) {
  DfourierTransform<-fourier(DiffTrainingTS, K=c(j))#create the fourier transforms using i transforms for monthly
  Dfit<-auto.arima(DiffTrainingTS,xreg=DfourierTransform,seasonal=FALSE) #Could also try seasonal=TRUE
  Dresults<-data.frame(j,Dfit$aicc)
  Daics<-rbind(Daics,Dresults)
  if(Dfit$aicc < Dbestfit$aicc){
    Dbestfit <- Dfit
    bestj<-j
    
  }
}

#Forecast Using Your Best Fourier Transforms####
#create fourier transform for best parameters
DfourierBest<-fourier(DiffTrainingTS, K=c(bestj))
DmodelFourier <- auto.arima(DiffTrainingTS,xreg=DfourierBest,seasonal=FALSE)

DFourierRSME <- sqrt(mean((DmodelFourier$residuals)^2))

DresultsFourier <- c(bestj,DmodelFourier$aic,DFourierRSME,NA)
DFourierParameters[nrow(DFourierParameters)+1,] = DresultsFourier

## Residuals and Fitted Values

DFourResiduals <- as.vector(residuals(DmodelFourier))
DFourResidualsDF <- as.data.frame(DFourResiduals)

DFourfittedValues<-as.vector(fitted(DmodelFourier))
DFourresidVsFitted <- data.frame(DFourResiduals,DFourfittedValues)
DFourfittedDF<-data.frame(DiffTraining$Time,DFourfittedValues)
names(DFourfittedDF)=c("Time","fittedValues")

## 4-in-1 Plots

#Normal Probability Plot
gDFourNormProb=ggplot(DFourResidualsDF,aes(sample=DFourResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gDFourFittedVsResid = ggplot(data=DFourresidVsFitted, aes(x=DFourfittedValues,y=DFourResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gDFourResidHist = ggplot(DFourResidualsDF,aes(x=DFourResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gDFourResidVsOrder =ggplot(data=DFourResidualsDF, aes(x=rownames(DFourResidualsDF), y = DFourResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


DFour4In1 = grid.arrange(gDFourNormProb, gDFourFittedVsResid,gDFourResidHist, gDFourResidVsOrder,nrow=2,ncol=2)

# Forecast 
DfourierFuture<-fourier(DiffTrainingTS, K=c(bestj),h=168)#create the future fourier values
DforecastFourier<-forecast(DmodelFourier,xreg=DfourierFuture,h=168)

#match forecasts with dates
DforecastFrameFourier<-data.frame(Time=DiffTest$Time,Forecast=as.numeric(DforecastFourier$mean))

DmapeFourier=mean(abs(DforecastFourier$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DmapeFourier = round(DmapeFourier, digits = 3)

DFourierParameters[1,4] <- DmapeFourier

#Make fancy plots
DupperCIFourierdf = data.frame(Time=DiffTest$Time,upperCI=as.numeric(DforecastFourier$upper))
DupperCIFourierdf<- DupperCIFourierdf[-c(169:nrow(DupperCIFourierdf)),]
DlowerCIFourierdf = data.frame(Time=DiffTest$Time,lowerCI=as.numeric(DforecastFourier$lower))
DlowerCIFourierdf<- DlowerCIFourierdf[-c(169:nrow(DlowerCIFourierdf)),]

titleString8 = paste("Difficult Test Set Forecasts - Fourier (Best i =",bestj,")")

colors4 <- c("Fourier Forecast" = "red","Actuals"="black", "95% CI"="blue")  #define colors for the graph to be used later
gDTrainAndForecastFourier<-ggplot(DiffTest, aes(x=Time,y=NetDemand,colour="Actuals"))+
  geom_point()+
  geom_line(data=DforecastFrameFourier,aes(x=Time,y=Forecast,colour="Fourier Forecast"))+
  geom_line(data=DupperCIFourierdf,aes(x=Time,y=upperCI,colour="95% CI"))+
  geom_line(data=DlowerCIFourierdf,aes(x=Time,y=lowerCI,colour="95% CI"))+
  geom_point(size = 1)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  annotate(geom="text",x=DiffTest[2,1],y=4500,label=paste("MAPE=",DmapeFourier))+
  labs(title = titleString8, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors4) +labs(colour="Legend")
gDTrainAndForecastFourier

