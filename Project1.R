library(zoo)
library(imputeTS)
library(plyr)
library(ggplot2)
library(scales)
library(gridExtra)

RawElectricityData <-read.table(file="201806 202112 Raw Electricity Data.csv",header=T,sep=",")

RawElectricityData$Time<- as.POSIXct(RawElectricityData$Time,tz="","%m/%d/%Y %H:%M")

start <- as.POSIXct('2018-06-01 00:00:00',tz='EST')  #this is used in line 27
end <- as.POSIXct('2021-12-31 23:55:00',tz='EST')    #this is used in line 27

#Lets make a condition that returns 5 minute, 30 minute, 60 minute intervals
#Here I use 5 minutes interval
interval<-5 

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


#evenlySpacedElectricity <- join(time,RawElectricityData, by = "Time", type = "left", match = "first")

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

#I first plot for 2 years
startDate1 = as.POSIXct("2018-06-01 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2018
endDate1 =as.POSIXct("2020-01-31 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2018
xAxislimits1 <- c(startDate1, endDate1) #we will use these x axis limits in our new graph below


g1 = ggplot(evenlySpacedElectricity, aes(x=Time,y=BuildingDemand_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for demand of buildingA 2018-2019 (2years)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 month"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))## to change the orientation of x axis title
g1

#It seems that we have some daily seasonality, so for analyzing this I plot 1week demand from Monday to Friday
#I consider a week from Nov-2021
startDate2 = as.POSIXct("2021-11-15 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2019
endDate2 =as.POSIXct("2021-11-19 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2019
xAxislimits2 <- c(startDate2, endDate2) #we will use these x axis limits in our new graph below


g2 = ggplot(evenlySpacedElectricity, aes(x=Time,y=BuildingDemand_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for demand of buildingA (1week)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y %H:%M"),breaks = date_breaks("12 hour"),limits = xAxislimits2) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))## to change the orientation of x axis title
g2
#Here we see daily seasonality

startDate3 = as.POSIXct("2020-09-02 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2021
endDate3 =as.POSIXct("2020-09-02 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2021
xAxislimits3 <- c(startDate3, endDate3) #we will use these x axis limits in our new graph below

g3 = ggplot(evenlySpacedElectricity, aes(x=Time,y=BuildingDemand_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for demand of buildingA (1day)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%H:%M"),breaks = date_breaks("1 hour"),limits = xAxislimits3) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## to change the orientation of x axis title
g3

#For more investigation I plot 3 weeks graph
startDate4 = as.POSIXct("2019-02-04 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2020
endDate4 =as.POSIXct("2019-02-25 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2020
xAxislimits4 <- c(startDate4, endDate4) #we will use these x axis limits in our new graph below

g4 = ggplot(evenlySpacedElectricity, aes(x=Time,y=BuildingDemand_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for demand of buildingA (3weeks)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits4) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))## to change the orientation of x axis title
g4

grid.arrange(g1,g4,g2, nrow=3)

#time series plot for solar data
#Here I consider the solar data for building A because it is not mentioned in the project tasks that which
#solar plan we should investigate

f = ggplot(evenlySpacedElectricity, aes(x=Time,y=SolarF01_A))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for solar data") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("90 days")) 
f

startDate1 = as.POSIXct("2018-06-01 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2018
endDate1 =as.POSIXct("2021-06-30 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2018
xAxislimits1 <- c(startDate1, endDate1) #we will use these x axis limits in our new graph below


f1 = ggplot(evenlySpacedElectricity, aes(x=Time,y=SolarF01_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for solarA data 2018-2021 (3years)") +
  theme(plot.title = element_text(hjust = 0.5,size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 month"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 2000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
f1

startDate2 = as.POSIXct("2021-11-15 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2019
endDate2 =as.POSIXct("2021-11-19 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2019
xAxislimits2 <- c(startDate2, endDate2) #we will use these x axis limits in our new graph below


f2 = ggplot(evenlySpacedElectricity, aes(x=Time,y=SolarF01_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for solarA data (1week)") +
  theme(plot.title = element_text(hjust = 0.5,size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y %H:%M"),breaks = date_breaks("12 hour"),limits = xAxislimits2) +
  scale_y_continuous(limits=c(0, 2000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
f2

startDate3 = as.POSIXct("2020-09-02 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2020
endDate3 =as.POSIXct("2020-09-02 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2020
xAxislimits3 <- c(startDate3, endDate3) #we will use these x axis limits in our new graph below


f3 = ggplot(evenlySpacedElectricity, aes(x=Time,y=SolarF01_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for solarA data at 2020 (1 day)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%H:%M"),breaks = date_breaks("1 hour"),limits = xAxislimits3) +
  scale_y_continuous(limits=c(0, 2000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## to change the orientation of x axis title
f3

startDate4 = as.POSIXct("2019-02-04 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2021
endDate4 =as.POSIXct("2019-02-25 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2021
xAxislimits4 <- c(startDate4, endDate4) #we will use these x axis limits in our new graph below


f4 = ggplot(evenlySpacedElectricity, aes(x=Time,y=SolarF01_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for solarA data at 2021 (3weeks)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits4) +
  scale_y_continuous(limits=c(0, 2000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## to change the orientation of x axis title
f4

grid.arrange(f1,f2, nrow=2)

#time series plot for net demand data

evenlySpacedElectricity$net_demand <- (evenlySpacedElectricity$BuildingDemand_A)-(evenlySpacedElectricity$SolarF01_A)
summary(evenlySpacedElectricity) #There is no negative value in net demand
var(evenlySpacedElectricity$BuildingDemand_A,na.rm=TRUE)
var(evenlySpacedElectricity$SolarF01_A,na.rm=TRUE)
var(evenlySpacedElectricity$net_demand,na.rm=TRUE)

n = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demand ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("120 days")) 
n

startDate1 = as.POSIXct("2018-06-01 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2018
endDate1 =as.POSIXct("2020-01-31 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2018
xAxislimits1 <- c(startDate1, endDate1) #we will use these x axis limits in our new graph below


n1 = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demandA 2018-2019 (2years)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 month"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
n1

startDate2 = as.POSIXct("2021-11-15 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2019
endDate2 =as.POSIXct("2021-11-19 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2019
xAxislimits2 <- c(startDate2, endDate2) #we will use these x axis limits in our new graph below


n2 = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demandA (1week)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y %H:%M"),breaks = date_breaks("12 hour"),limits = xAxislimits2) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
n2

startDate3 = as.POSIXct("2020-09-02 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2020
endDate3 =as.POSIXct("2020-09-02 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2020
xAxislimits3 <- c(startDate3, endDate3) #we will use these x axis limits in our new graph below


n3 = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demandA (1 day)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%H:%M"),breaks = date_breaks("1 hour"),limits = xAxislimits3) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## to change the orientation of x axis title
n3

startDate4 = as.POSIXct("2019-02-04 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2021
endDate4 =as.POSIXct("2019-02-25 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2021
xAxislimits4 <- c(startDate4, endDate4) #we will use these x axis limits in our new graph below


n4 = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demandA (3weeks)") +
  theme(plot.title = element_text(hjust = 0.5, size='32')) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits4) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
n4

grid.arrange(n1,n4,n2, nrow=3)

#*****************************************
#2d. Finding Summary stats of your data
#****************************************
summary(evenlySpacedElectricity) #will give mean, median and quartiles
var(evenlySpacedElectricity$net_demand,na.rm=TRUE) #will calculate the variance and remove NAs
sd(evenlySpacedElectricity$net_demand,na.rm=TRUE)#will calculate standard deviation and remove NAs

#*****************************************
#Plot the data
#****************************************
g = ggplot(evenlySpacedElectricity, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demand of buildingA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("90 days")) 
g

#lets replace all zero values with NAs

evenlySpacedElectricity$net_demand[evenlySpacedElectricity$net_demand<=0]<-NA
#lets replace all high values with NAs
#evenlySpacedData$Demand[evenlySpacedData$Demand>15000]<-NA
#let's use the imputation library to impute all the NA's 
summary(evenlySpacedElectricity)
var(evenlySpacedElectricity$net_demand,na.rm=TRUE)

#first let's see where the NAs are
ggplot_na_distribution(evenlySpacedElectricity$net_demand)+
  labs(x = "Time", y = "Demand (kW)")+
  theme(plot.title = element_text(size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
#we can also get some summary statistics about our NAs
statsNA(evenlySpacedElectricity$net_demand,bins=5)

#linear interpolation by connecting points on either side of missing value
imputedLinear<-na_interpolation(evenlySpacedElectricity$net_demand,option="linear")

#lets join our imputed series back to their time values in new data frames
imputedLinear <- data.frame(evenlySpacedElectricity$Time,imputedLinear)
colnames(imputedLinear)<-c("Time","net_demand")
summary(imputedLinear)
var(imputedLinear$net_demand,na.rm=TRUE)

#lets plot our imputedLinear data
gInputedLinear = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for Linearly Imputed Electricity Net Demand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("3 months")) 
gInputedLinear

#lets plot our imputations for the month 5,6 of 2020
startDate = as.POSIXct("2020-06-15 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-06-15 23:55",tz="","%Y-%m-%d %H:%M")
withNas = evenlySpacedElectricity[evenlySpacedElectricity$Time>=startDate&evenlySpacedElectricity$Time<=endDate,]

cleaned = imputedLinear[imputedLinear$Time>=startDate&imputedLinear$Time<=endDate,]
Linear56= ggplot_na_imputations ((withNas$net_demand), (cleaned$net_demand),color_points = "black")+
  labs(x = "Time", y = "Demand (kW)", title = "Linearly Imputed Electricity Net Demand for one day in June")+
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))## to change the orientation of x axis title
Linear56
#moving average imputation by taking average of last k values
imputedMovingAverage = na_ma(evenlySpacedElectricity$net_demand,k=4,weighting="simple")

#lets join our imputed series back to their time values in new data frames
imputedMovingAverage <- data.frame(evenlySpacedElectricity$Time,imputedMovingAverage)
colnames(imputedMovingAverage)<-c("Time","net_demand")

summary(imputedMovingAverage)
var(imputedMovingAverage$net_demand,na.rm=TRUE)

#lets plot our imputedMovingAverage data
gImputedMovingAverager = ggplot(imputedMovingAverage, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for Moving Average Imputed Electricity Net Demand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("120 days")) 
gImputedMovingAverager

#lets plot our imputations for the month 5,6 of 2020
startDate = as.POSIXct("2020-06-15 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-06-15 23:55",tz="","%Y-%m-%d %H:%M")
withNas = evenlySpacedElectricity[evenlySpacedElectricity$Time>=startDate&evenlySpacedElectricity$Time<=endDate,]

cleaned = imputedMovingAverage[imputedMovingAverage$Time>=startDate&imputedMovingAverage$Time<=endDate,]
MovingAverage56= ggplot_na_imputations ((withNas$net_demand), (cleaned$net_demand), color_points = "black")+
  labs(x = "Time", y = "Demand (kW)", title = "Moving Average Imputed Electricity Net Demand for one day in June")+
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
MovingAverage56

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



#lets plot our imputations for the second week of april
startDate = as.POSIXct("2020-06-15 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-06-15 23:55",tz="","%Y-%m-%d %H:%M")
withNas = evenlySpacedElectricity[evenlySpacedElectricity$Time>=startDate&evenlySpacedElectricity$Time<=endDate,]
cleaned = imputedLVCF[imputedLVCF$Time>=startDate&imputedLVCF$Time<=endDate,]
LVCF56= ggplot_na_imputations(withNas$net_demand,cleaned$net_demand, color_points = "black")+
  labs(x = "Time", y = "Demand (kW)", title = "LVCF Imputed Electricity Net Demand for one day in June")+
  theme(panel.background = element_rect(fill = 'Light Blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30')) ## to change the orientation of x axis title
LVCF56

grid.arrange(gInputedLinear,gImputedMovingAverager,gImputedLVCF, nrow=3)
grid.arrange(Linear56,MovingAverage56,LVCF56, nrow=3)

#plot moving average for this data series in blue (12 time period rolling window)

movingAverage12Periods = data.frame(imputedLinear$Time[12:nrow(imputedLinear)],rollmean(imputedLinear$net_demand,12))
colnames(movingAverage12Periods) = c('Time','net_demand')

movingAverage6Periods = data.frame(imputedLinear$Time[6:nrow(imputedLinear)],rollmean(imputedLinear$net_demand,6))
colnames(movingAverage6Periods) = c('Time','net_demand')
#Add the moving average series to the graphs we just made
#Plot 1 Month (February 2020)

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-28 23:55",tz="","%Y-%m-%d %H:%M")
gMonth = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "February Electricity Demand-Moving Average") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("7 days"),limits = c(startDate,endDate)) 
gMonth
gMonthAvg = gMonth+geom_line(data=movingAverage12Periods, aes(x=Time,y=net_demand), color='blue')
gMonthAvg

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-08 23:55",tz="","%Y-%m-%d %H:%M")
gWeek = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "One Week February Electricity Demand-Moving Average") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("1 days"),limits = c(startDate,endDate)) 
gWeek
gWeekAvg = gWeek + geom_line(data=movingAverage12Periods, aes(x=Time,y=net_demand), color='blue')
gWeekAvg

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-02 23:55",tz="","%Y-%m-%d %H:%M")
gDay = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Average") +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))+ ## to change the orientation of x axis title
  scale_x_datetime(labels=date_format("%Y-%m-%d %H:%M"),breaks = date_breaks("12 hours"),limits = c(startDate,endDate)) 
gDay
gDayAvg12 = gDay + geom_line(data=movingAverage12Periods, aes(x=Time,y=net_demand), color='blue')+
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Average N=12")
gDayAvg12
gDayAvg6 = gDay + geom_line(data=movingAverage6Periods, aes(x=Time,y=net_demand), color='blue')+
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Average N=6")
gDayAvg6

grid.arrange(gMonthAvg,gWeekAvg,gDayAvg12,nrow=3)
grid.arrange(gDayAvg6,gDayAvg12,nrow=2)
#plot moving Median for this data series in blue (12 time period rolling window)

movingMedian12Periods = data.frame(imputedLinear$Time[12:nrow(imputedLinear)],rollmedian(imputedLinear$net_demand,12))
colnames(movingMedian12Periods) = c('Time','net_demand')

movingMedian6Periods = data.frame(imputedLinear$Time[6:nrow(imputedLinear)],rollmedian(imputedLinear$net_demand,6))
colnames(movingMedian6Periods) = c('Time','net_demand')
#Add the moving average series to the graphs we just made
#Plot 1 Month (February 2020)

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-28 23:55",tz="","%Y-%m-%d %H:%M")
sMonth = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "February Electricity Demand-Moving Median") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("7 days"),limits = c(startDate,endDate)) 
sMonth
sMonthMedian = sMonth+geom_line(data=movingMedian12Periods, aes(x=Time,y=net_demand), color='red')
sMonthMedian

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-08 23:55",tz="","%Y-%m-%d %H:%M")
sWeek = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "One Week February Electricity Demand-Moving Median") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("1 days"),limits = c(startDate,endDate)) 
sWeek
sWeekMedian = sWeek + geom_line(data=movingMedian12Periods, aes(x=Time,y=net_demand), color='red')
sWeekMedian

startDate = as.POSIXct("2020-02-01 00:00",tz="","%Y-%m-%d %H:%M")
endDate =as.POSIXct("2020-02-02 23:55",tz="","%Y-%m-%d %H:%M")
sDay = ggplot(imputedLinear, aes(x=Time,y=net_demand))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Median") +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))+
  scale_x_datetime(labels=date_format("%Y-%m-%d"),breaks = date_breaks("1 days"),limits = c(startDate,endDate)) 
sDay
sDayMedian12 = sDay + geom_line(data=movingMedian12Periods, aes(x=Time,y=net_demand), color='red')+
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Median N=12")
sDayMedian12

sDayMedian6 = sDay + geom_line(data=movingMedian6Periods, aes(x=Time,y=net_demand), color='red')+
  labs(x = "Time", y = "Demand (kW)", title = "One Day February Electricity Demand-Moving Median N=6")
sDayMedian6

grid.arrange(sMonthMedian,sWeekMedian,sDayMedian,nrow=3)
grid.arrange(sDayMedian12,sDayMedian6,nrow=2)
grid.arrange(gDayAvg12,sDayMedian12,nrow=2)
grid.arrange(gDayAvg6,sDayMedian6,nrow=2)


#Removing trend and seasonality

startDate = as.POSIXct("2019-02-05 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2021
endDate =as.POSIXct("2019-02-26 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2021
xAxislimits <- c(startDate, endDate) #we will use these x axis limits in our new graph below


original = ggplot(imputedLinear, aes(x=Time,y=net_demand)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for net demandA (3weeks)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
original

ACF_original <- ggAcf(imputedLinear$net_demand, lag=1000)+
  labs(x = "Lag", y = "ACF", title = "ACF plot of cleaned data (3weeks)")+
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
ACF_original

firstDiffSineData <-data.frame(firstDiff = diff(imputedLinear$net_demand,lag=2016))

firstDiffSineData<-cbind(x=imputedLinear$Time[2017:377280],firstDiff=firstDiffSineData)

firstDiffPlotSine = ggplot(firstDiffSineData, aes(x=x,y=firstDiff)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black')) +
  labs(x = "Date", y = "Demand (kW)", title = "Difference Lag 2016 to Remove Seasonality") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(-500, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
firstDiffPlotSine 

ACF_deseasoned <- ggAcf(firstDiffSineData$firstDiff, lag=1000)+
  labs(x = "Lag", y = "ACF", title = "Difference Lag 2016 to Remove Seasonality")+
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
ACF_deseasoned

summary(firstDiffSineData)
var(firstDiffSineData$firstDiff)

#remove trend

firstDiffRemoveTrend <-data.frame(firstDiff = diff(imputedLinear$net_demand,lag=1))

#notice how first Diff has 1 fewer entries than our originalData
firstDiffRemoveTrend<-cbind(x=imputedLinear$Time[2:377280],firstDiff=firstDiffRemoveTrend)


firstDiffPlotTrend = ggplot(firstDiffRemoveTrend, aes(x=x,y=firstDiff)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black')) +
  labs(x = "Date", y = "Demand (kW)", title = "Difference Lag 1 to Remove Trend") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(-500, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
firstDiffPlotTrend 

ACF_detrended <- ggAcf(firstDiffRemoveTrend$firstDiff, lag=1000)+
  labs(x = "Lag", y = "ACF", title = "Difference Lag 1 to Remove Trend")+
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
ACF_detrended
summary(firstDiffRemoveTrend)
var(firstDiffRemoveTrend$firstDiff)
#remove both

#first remove seasonality then trend exercise
firstDiffSineData <-data.frame(firstDiff = diff(imputedLinear$net_demand,lag=1))

firstDiffSineData<-cbind(x=imputedLinear$Time[2:377280],firstDiff=firstDiffSineData)


diffTrendAndSeasonality <-data.frame(x=imputedLinear$Time[2018:377280],secondDiff=diff(firstDiffSineData$firstDiff,lag=2016))
summary(diffTrendAndSeasonality)
var(diffTrendAndSeasonality$secondDiff)

diffPlotTrendandSeasonality = ggplot(diffTrendAndSeasonality, aes(x=x,y=secondDiff)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black')) +
  labs(x = "Date", y = "Demand (kW)", title = "Difference to Remove Seasonality and Trend") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 day"),limits = xAxislimits) +
  scale_y_continuous(limits=c(-500, 500)) +
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))
diffPlotTrendandSeasonality

grid.arrange(original,firstDiffPlotSine,firstDiffPlotTrend,diffPlotTrendandSeasonality,nrow=2,ncol=2)

ACF_both <- ggAcf(diffTrendAndSeasonality$secondDiff, lag=1000)+
  labs(x = "Lag", y = "ACF", title = "Difference to Remove Seasonality and Trend")+
  theme(panel.background = element_rect(fill = 'Light blue', colour = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size='32'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size='12'))+
  theme(axis.text.y=element_text(size='12'))+
  theme(axis.title=element_text(size='30'))

grid.arrange(ACF_original,ACF_deseasoned,ACF_detrended,ACF_both,nrow=2,ncol=2)

