# Project 3- ARIMA difficult week only
# Team Member: Sonia, Saleh, Vineet

library(ggplot2)
library(gridExtra)
library(TSstudio)
library(zoo)
library(scales)
library(forecast)

hrlyData <- read.table("hrlyData.csv",header=TRUE,sep=',')
hrlyData$Time <- as.POSIXct(hrlyData$Time,tz="",format = "%Y-%m-%d %H:%M:%S")

EasyTraining <- hrlyData[hrlyData$Time<"2021-02-02 00:00:00"& hrlyData$Time>="2020-11-01 00:00:00",c("Time","NetDemand")]
EasyTraining <- EasyTraining[c(-1,-2234),]

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



# Difficult training ARIMA Model

# Model 1

p2=3
d2=1
q2=1
P2=0
D2=1
Q2=2


Dmodel2 <- Arima(DiffTraining$NetDemand,order=c(p2,d2,q2),seasonal=list(order=c(P2,D2,Q2),period=168),method="ML")
Dmodel2


DRMSE2 <- sqrt(mean((Dmodel2$residuals)^2))
Dresults2 <- c(p,d,q,P,D,Q,Dmodel2$aic,DRMSE2,NA)
DmodelParameters[nrow(DmodelParameters)+1,] = Dresults2


#MODEL 2
p=9
d=1
q=1
P=0
D=1
Q=1


Dmodel <- Arima(DiffTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="ML")
Dmodel


DRMSE <- sqrt(mean((Dmodel$residuals)^2))
Dresults <- c(p,d,q,P,D,Q,Dmodel$aic,DRMSE,NA)
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

## Forecast for the Difficult Test Set

DForecast <- forecast(Dmodel,h=168)

DMAPEArima <-mean(abs(DForecast$mean - DiffTest$NetDemand)/DiffTest$NetDemand)*100
DMAPEArima = round(DMAPEArima, digits = 3)  

DmodelParameters[1,9] <- DMAPEArima

DfittedData <- data.frame(Time= DiffTraining$Time, NetDemand = DForecast$fitted)
DforecastValue <- data.frame(Time = DiffTest$Time, NetDemand = DForecast$mean)
DforecastUpper95 <- data.frame(Time = DiffTest$Time, Upper = DForecast$upper[,2])
DforecastLower95 <- data.frame(Time = DiffTest$Time, Lower = DForecast$lower[,2])

titleString9 = paste("Difficult  Test Forecasts - ARIMA(",p,",",d,",",q,")X(",P,",",D,",",Q,")")

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


#MODEL 3
p1=4
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

#MODEL 4

#modelArima <- auto.arima(EasyTrainingTS,seasonal=TRUE)
#modelArima
#Emodel <- arima(EasyTraining$NetDemand,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=168),method="CSS")
p3=4
d3=1
q3=1
P3=0
D3=0
Q3=0
Dmodel3 <- auto.arima(DiffTrainingTS,seasonal =TRUE)
Dmodel3


DRMSE3 <- sqrt(mean((Dmodel3$residuals)^2))
Dresults3 <- c(p3,d3,q3,P3,D3,Q3,Dmodel3$aic,DRMSE3,NA)
DmodelParameters[nrow(DmodelParameters)+1,] = Dresults3
