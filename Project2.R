
library(zoo)
library(imputeTS)
library(plyr)
library(ggplot2)
library(scales)
library(gridExtra)
library("forecast")
library(ggplot2)
#install.packages("tsbox") # for manipulating time series objects
library(tsbox)
library(lubridate)
#install.packages("ggfortify")
library("ggfortify")
dev.off()

RawElectricityData <-read.table(file="201806 202112 Raw Electricity Data.csv",header=T,sep=",")

RawElectricityData$Time<- as.POSIXct(RawElectricityData$Time,tz="","%m/%d/%Y %H:%M")

start <- as.POSIXct('2018-06-01 00:00:00',tz='EST')  #this is used in line 27
end <- as.POSIXct('2021-12-31 23:55:00',tz='EST')    #this is used in line 27

#Lets make a condition that returns 5 minute, 30 minute, 60 minute intervals
#Here I use 5 minutes interval
interval<-60 

if(interval==5){
  time =data.frame( seq.POSIXt(from = start, to =end , by = "5 min"))
  colnames(time)= c("Time") # name the column "time"
  evenlySpacedElectricity <- join(time,RawElectricityData, by = "Time", type = "left", match = "first")
} else if(interval==30) {
  time =data.frame( seq.POSIXt(from = start, to =end , by = "30 min"))
  colnames(time)= c("Time") # name the column "time" 
  evenlySpacedElectricity <- join(time,RawElectricityData, by = "Time", type = "left", match = "first")
} else {
  time =data.frame( seq.POSIXt(from = start, to =end , by = "60 min"))
  colnames(time)= c("Time") # name the column "time" 
  evenlySpacedElectricity <- join(time,RawElectricityData, by = "Time", type = "left", match = "first")
}


#*****************************************
#2d. Finding Summary stats of your data
#****************************************
summary(evenlySpacedElectricity) #will give mean, median and quartiles
var(evenlySpacedElectricity$BuildingDemand_A,na.rm=TRUE) #will calculate the variance and remove NAs
sd(evenlySpacedElectricity$BuildingDemand_A,na.rm=TRUE)#will calculate standard deviation and remove NAs
var(evenlySpacedElectricity$SolarF01_A,na.rm=TRUE)

#*****************************************
#Plot the data
#****************************************
g = ggplot(evenlySpacedElectricity, aes(x=Time,y=BuildingDemand_A))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for buildingA Electric Demand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("90 days")) 
g

#net demand data

evenlySpacedElectricity$net_demand <- (evenlySpacedElectricity$BuildingDemand_A)-(evenlySpacedElectricity$SolarF01_A)
summary(evenlySpacedElectricity) #There is no negative value in net demand
var(evenlySpacedElectricity$BuildingDemand_A,na.rm=TRUE)
var(evenlySpacedElectricity$SolarF01_A,na.rm=TRUE)
var(evenlySpacedElectricity$net_demand,na.rm=TRUE)

#lets replace all zero values with NAs

evenlySpacedElectricity$net_demand[evenlySpacedElectricity$net_demand<=0]<-NA
evenlySpacedElectricity$BuildingDemand_A[evenlySpacedElectricity$BuildingDemand_A<=0]<-NA
evenlySpacedElectricity$SolarF01_A[evenlySpacedElectricity$SolarF01_A<=0]<-NA

#*****************************************
#Plot the net demand data
#****************************************
g = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for buildingA Net Demand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("90 days")) 
g

#impute last value carried forward
imputedLVCF = na_locf(evenlySpacedElectricity$net_demand)

#lets join our imputed series back to their time values in new data frames
imputedLVCF <- data.frame(evenlySpacedElectricity$Time,imputedLVCF)
colnames(imputedLVCF)<-c("Time","net_demand")

summary(imputedLVCF)
var(imputedLVCF$net_demand,na.rm=TRUE)

#lets plot our imputedLinear data
gImputedLVCF = ggplot(imputedLVCF, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for Imputed Electricity Net Demand by LVCF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("120 days")) 
gImputedLVCF


#lets plot our imputations for the difficult test week of December
DifficultTestStart = as.POSIXct("2019-12-21 00:00",tz="","%Y-%m-%d %H:%M")
DifficultTestEnd =as.POSIXct("2019-12-28 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits <- c(DifficultTestStart, DifficultTestEnd) #we will use these x axis limits in our new graph below

n1 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for difficult test week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n1

#lets plot our imputations for the training set of difficult week
DifficultTrainStart = as.POSIXct("2019-03-01 00:00",tz="","%Y-%m-%d %H:%M")
DifficultTrainEnd =as.POSIXct("2019-12-20 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits1 <- c(DifficultTrainStart, DifficultTrainEnd) #we will use these x axis limits in our new graph below

n2 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for training set of difficult week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("30 day"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n2

#lets plot our imputations for the validation set of difficult week
DifficultValidStart = as.POSIXct("2019-12-21 00:00",tz="","%Y-%m-%d %H:%M")
DifficultValidEnd =as.POSIXct("2019-12-27 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits2 <- c(DifficultValidStart, DifficultValidEnd) #we will use these x axis limits in our new graph below

n3 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for validation set of difficult week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits2) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n3

grid.arrange(n1,n3,n2, nrow=3)



#lets plot our imputations for the easy test week
EasyTestStart = as.POSIXct("2021-09-13 00:00",tz="","%Y-%m-%d %H:%M")
EasyTestEnd =as.POSIXct("2021-09-19 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits <- c(EasyTestStart, EasyTestEnd) #we will use these x axis limits in our new graph below

n4 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for difficult test week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n4

#lets plot our imputations for the training set of difficult week
EasyTrainstart = as.POSIXct("2019-01-01 00:00",tz="","%Y-%m-%d %H:%M")
EasyTrainEnd =as.POSIXct("2019-09-15 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits1 <- c(EasyTrainstart, EasyTrainEnd) #we will use these x axis limits in our new graph below

n5 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for training set of difficult week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("30 day"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n5

#lets plot our imputations for the validation set of difficult week
EasyValidStart = as.POSIXct("2019-09-16 00:00",tz="","%Y-%m-%d %H:%M")
EasyValidEnd =as.POSIXct("2019-09-22 23:55",tz="","%Y-%m-%d %H:%M")
xAxislimits2 <- c(EasyValidStart, EasyValidEnd) #we will use these x axis limits in our new graph below

n6 = ggplot(imputedLVCF, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for validation set of difficult week") +
  theme(plot.title = element_text(hjust = 0.5, size='12')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits2) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='12')) ## to change the orientation of x axis title
n6

grid.arrange(n4,n6,n5, nrow=3)

#Defining test and train and validation set for difficult and easy weeks

DifficultTrainSet <- imputedLVCF[DifficultTrainStart<=imputedLVCF$Time & imputedLVCF$Time<= DifficultTrainEnd,]
DifficultValidSet <- imputedLVCF[DifficultValidStart<=imputedLVCF$Time & imputedLVCF$Time<=DifficultValidEnd,]
DifficultTestSet <- imputedLVCF[DifficultTestStart<=imputedLVCF$Time & imputedLVCF$Time<=DifficultTestEnd,]

EasyTrainSet <- imputedLVCF[EasyTrainstart<=imputedLVCF$Time & imputedLVCF$Time<= EasyTrainEnd,]
EasyValidSet <- imputedLVCF[EasyValidStart<=imputedLVCF$Time & imputedLVCF$Time<= EasyValidEnd,]
EasyTestSet <- imputedLVCF[EasyTestStart<=imputedLVCF$Time & imputedLVCF$Time<= EasyTestEnd,]

DifficultTrainingTS <- ts(DifficultTrainSet$net_demand,freq=168)
DifficultTrainingTS

EasyTrainTS <- ts(EasyTrainSet$net_demand,freq=168)
EasyTrainTS

summary(DifficultTrainingTS)
summary(EasyTrainTS)

ggplot_na_distribution(DifficultTrainingTS, color_missing = "indianred") #(See missing data)

hwDifficult<-HoltWinters(DifficultTrainingTS,alpha=0.5, beta=0, gamma=0.1,seasonal="additive")
hwEasy<-HoltWinters(EasyTrainTS,alpha=0.5, beta=0, gamma=0.1,seasonal="additive")

summary(hwDifficult)
plot(hwDifficult)

summary(hwEasy)

xDifficultTS = hwDifficult$fitted[,1]  #this retrieves the smoothed values as a time series
class(xDifficultTS)
xEasyTS = hwEasy$fitted[,1]  #this retrieves the smoothed values as a time series
class(xEasyTS)

xDifficultDF = ts_df(xDifficultTS) #converts time series object to data frame (for easier graphing)
xEasyDF = ts_df(xEasyTS) #converts time series object to data frame (for easier graphing)


#CHANGE TIMES TO BE FROM ORIGINAL TRAINING SET
xDifficultDF$time<-DifficultTrainSet$Time[169:7080]
xEasyDF$time<-EasyTrainSet$Time[169:6335]

colors <- c("Actual" = "black","Winters"="blue")  #define colors for the graph to be used later
g1<-ggplot(xDifficultDF, aes(x=time,y=value,color = "Winters"))+
  labs(y="Demand", x="Date",title="Winters Smoothed Training Data for Difficult Week")+
  geom_line(size = 1)+
  geom_point(data=DifficultTrainSet,aes(x=Time,y=net_demand,color="Actual"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 month",date_labels = "%b-%y")+
  scale_colour_manual(values= colors)+labs(colour="Legend")
g1

colors <- c("Actual" = "black","Winters"="blue")  #define colors for the graph to be used later
g2<-ggplot(xEasyDF, aes(x=time,y=value,color = "Winters"))+
  labs(y="Demand", x="Date",title="Winters Smoothed Training Data for Easy Week")+
  geom_line(size = 1)+
  geom_point(data=EasyTrainSet,aes(x=Time,y=net_demand,color="Actual"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 month",date_labels = "%b-%y")+
  scale_colour_manual(values= colors)+labs(colour="Legend")
g2

#Difficult model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("Alpha","Beta","Gamma","SSE","MSE","RMSE","MAE Validation","MAPE Validation","Test MAE","Test MAPE")
DifficultmodelResults = data.frame(matrix(ncol=10,nrow=0))
colnames(DifficultmodelResults)=columnNames

SSE = hwDifficult$SSE
MSE = hwDifficult$SSE/length(DifficultTrainSet)
print(paste("MSE=",MSE))
RMSE = sqrt(MSE)
print(paste("RMSE=",RMSE))
#Forecast Validation set for difficult
DifficultValidTS<-ts(DifficultValidSet$net_demand,freq=168)
length(DifficultValidTS)
DifficultValidForecast<-predict(hwDifficult,n.ahead=168,prediction.interval=TRUE)
MAE <- mean(abs(DifficultValidForecast[,1]-DifficultValidSet$net_demand))
MAPE<- mean(abs(DifficultValidForecast[,1]-DifficultValidSet$net_demand)/DifficultValidSet$net_demand)*100

results = c(hwDifficult$alpha,hwDifficult$beta,hwDifficult$gamma,hwDifficult$SSE,MSE,RMSE,MAE,MAPE,NA,NA)
DifficultmodelResults[nrow(DifficultmodelResults)+1,] = results

#Easy model performance
#Let's create an empty data frame to keep track of our training and validation results
columnNames = c("Alpha","Beta","Gamma","SSE","MSE","RMSE","MAE Validation","MAPE Validation","Test MAE","Test MAPE")
EasymodelResults = data.frame(matrix(ncol=10,nrow=0))
colnames(EasymodelResults)=columnNames

SSE = hwEasy$SSE
MSE = hwEasy$SSE/length(EasyTrainSet)
print(paste("MSE=",MSE))
RMSE = sqrt(MSE)
print(paste("RMSE=",RMSE))

EasyValidTS<-ts(EasyValidSet$net_demand,freq=168)
length(EasyValidTS)

EasyValidForecast<-predict(hwEasy,n.ahead=168,prediction.interval=TRUE)
MAE <- mean(abs(EasyValidForecast[,1]-EasyValidSet$net_demand))
MAPE<- mean(abs(EasyValidForecast[,1]-EasyValidSet$net_demand)/EasyValidSet$net_demand)*100

results = c(hwEasy$alpha,hwEasy$beta,hwEasy$gamma,hwEasy$SSE,MSE,RMSE,MAE,MAPE,NA,NA)
EasymodelResults[nrow(EasymodelResults)+1,] = results




##Plot the Forecast With CI Lines
#lets grab some things from our forecast we need for graphing (the ts_df turns the time series objects into regular data frames)
predictions = ts_df(BikeCountForecast2[,1])
predictions$time<-testSet$Date[1:168]

upperCI = ts_df(BikeCountForecast2[,2])
upperCI$time<-testSet$Date[1:168]

lowerCI = ts_df(BikeCountForecast2[,3])
lowerCI$time<-testSet$Date[1:168]

colors2 <- c("Actual" = "black","Winters Smoothed"="blue","Forecast"="Red","95% CI"="green")  #define colors for the graph to be used later

g<-ggplot(xhatDF2, aes(x=time,y=value,color = "Winters Smoothed"))+
  labs(y="BikeCount", x="Date",title="Winters Smoothed Training Data + Forecast")+
  geom_line(size = 1)+
  geom_point(data=trainingSet,aes(x=Date,y=West, color="Actual"))+
  geom_point(data=testSet,aes(x=Date,y=West, color="Actual"))+
  geom_line(data=predictions,aes(x=time,y=value, color="Forecast"))+
  geom_line(data=upperCI,aes(x=time,y=value, color="95% CI"))+
  geom_line(data=lowerCI,aes(x=time,y=value, color="95% CI"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 year",date_labels = "%b-%y")+
  scale_colour_manual(values= colors2)+labs(colour="Legend")
g

