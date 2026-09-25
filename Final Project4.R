
library(nnet)
library(ggplot2)
library(corrplot)

AllData = read.csv("201807 to 202203 RIT PELD Dataset - Student Copy.csv", na.strings = c("","NA"), header = T)

# Change dates and times column into POSIXct format.
AllData$Time = as.POSIXct(AllData$Time,tz="","%m/%d/%Y %H:%M")

# Check Time
print(summary(AllData$Time))

#Plot 2021
startDate1 = as.POSIXct("2021-01-01 00:00",tz="","%Y-%m-%d %H:%M") # start time of 2018
endDate1 =as.POSIXct("2021-12-31 23:55",tz="","%Y-%m-%d %H:%M")    # end time the end of year 2018
xAxislimits1 <- c(startDate1, endDate1) #we will use these x axis limits in our new graph below


g1 = ggplot(AllData, aes(x=Time,y=NetDemand_A)) + geom_line(size= 1 ,na.rm = TRUE) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for demand of buildingA 2021") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%m-%d-%Y"),breaks = date_breaks("1 month"),limits = xAxislimits1) +
  scale_y_continuous(limits=c(0, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## to change the orientation of x axis title
g1


#Seeing the number of Na values
sapply(AllData,function(x) sum(is.na(x)))


EasyTest <- AllData[AllData$Time>="2021-02-02 00:00:00"& AllData$Time<"2021-02-09 00:00:00",]
DiffTest <- AllData[AllData$Time>="2021-12-25 00:00:00"&AllData$Time<="2021-12-31 23:00:00",]
MarchLastWeek <- AllData[AllData$Time>="2022-03-25 00:00:00"&AllData$Time<="2022-03-31 23:00:00",]

#Seeing the number of Na values
sapply(MarchLastWeek,function(x) sum(is.na(x)))

#Defining all data sets without our test sets
allDataReady1 <- AllData[AllData$Time>="2018-07-01 00:00:00"& AllData$Time<"2021-02-02 00:00:00",]
allDataReady2 <- AllData[AllData$Time>="2021-02-09 00:00:00" & AllData$Time<"2021-12-25 00:00:00",]
allDataReady3 <- AllData[AllData$Time>="2022-01-01 00:00:00" & AllData$Time<"2022-03-25 00:00:00",]
#allDataReady4 <- AllData[AllData$Time>="2021-12-31 00:00:00" & AllData$Time<"2022-12-25 00:00:00",]

allDataReady <- rbind(allDataReady1,allDataReady2,allDataReady3)
#-------------------------------------------------------------------------------
# Let's prepare all your data for use, you can decide which variables to try
#Turn Day of Week, Month, Time, and Rain into Indicator Variables

dayOfWeekIndicator<-class.ind(allDataReady$DoW)
monthOfYearIndicator<-class.ind(allDataReady$Month)
timeIndicator<-class.ind(allDataReady$HoD)
dayofMonthIndicator<-class.ind(allDataReady$DoM)
RITopenIndicator<-class.ind(allDataReady$Ev_RITOpen)
SemesterIndicator<-class.ind(allDataReady$Semester)
ClassesOpenIndicator<-class.ind(allDataReady$Ev_Classes)

#group all indicators together using only 6 days of week, 11 months, 23 hours, and 1 rain column
allIndicators<-data.frame(dayOfWeekIndicator[,1:6],monthOfYearIndicator[,1:11],timeIndicator[,1:23],dayofMonthIndicator[,1:30],RITopen=RITopenIndicator[,2],
                          Semester=SemesterIndicator[,1:2],Classes=ClassesOpenIndicator[,1] )

#Now we need to grab and normalize the continuous variables
#First step is to load this normalization function
normalizefunction = function(x) {
  num = x - min(x)
  denom = max(x) - min(x)
  return(num/denom)
}
#We'll use this unnormalize function later (load it now by running the function)
unnormalizefunction = function(x,min,max) {
  return(x*(max-min)+min)
}

#keep track of max and min bikes to denormalize later
minNetDemand_A<-min(allDataReady$NetDemand_A)
maxNetDemand_A<-max(allDataReady$NetDemand_A)

#normalize all the continuous columns at once by using 'sapply'
allContinuous = allDataReady[,c("AW_ExtTemp","DayHighTemp","DayLowTemp","HeatReqCalc","CoolReqCalc",
                                "CW_CoolReq","CW_HeatReq","AW_FeelsLike","AW_WeatherClass","AW_ExtHum","AW_DewPoint","OA_WindSpe","OA_WindDir","NetDemand_A")]


normContinuous = data.frame(sapply(allContinuous, normalizefunction))

#Seeing the number of Na values
sapply(normContinuous,function(x) sum(is.na(x)))

#create data frame that has Date, All Predictors, and Bike Traffic
FinalData<-data.frame(allDataReady$Time,allIndicators,normContinuous)
colnames(FinalData)[1] <- "Time"


#The allDataReady data frame houses all your training data. We should split it up into training and validation sets
#We can use a cool selection method to select 80% of the data for training and 20% for validation

## 80% of the sample size
training_sample_size <- floor(0.80 * nrow(FinalData))

## set the seed to make your partition reproducible
set.seed(200)
#create a list as long as your data that indicates if the data is in the training set or not
train_ind <- sample(seq_len(nrow(FinalData)), size = training_sample_size)
#train_ind contains a list of length (training_sample_size) where each entry will be a row # of our training set

RIT.train <- FinalData[train_ind, ]  #this will grab all the rows listed in our train_ind list
RIT.validation <- FinalData[-train_ind, ] #this will grab everything not listed in our train_ind list


#########################################################################################
####################################We could also explore correlations with all variables##################
######You can use a correlation plot to eliminate variables that are highly correlated with each other
######Or you can identify variables highly correlated with your data you are trying to predict
#############################################################################################
frameForCor = RIT.train[,76:89]
colnames(frameForCor)=c("AW_ExtTemp","DayHighTemp","DayLowTemp","HeatReqCalc","CoolReqCalc",
                        "CW_CoolReq","CW_HeatReq","AW_FeelsLike","AW_WeatherClass","AW_ExtHum","AW_DewPoint","OA_WindSpe","OA_WindDir","NetDemand_A")
corrplot(cor(frameForCor),method='ellipse',order="AOE",type="upper")

#################################################Forecast#############################
h=9
d=0.001
maxIter=800

#based on the plots above, choose your final hidden node number to use.
#Retrain the model using these parameters on the ENTIRE TRAINING SEt (All data training and validation at once)
nnetFinalFit<-nnet(FinalData[,c(2:75,81,82,85,86,87,88)], # the regressor variables
                   FinalData[,89], #what you are trying to predict
                   size=h, #number of hidden nodes
                   decay = d, #gives a penalty for large weights
                   linout = TRUE, #says you want a linear output (as oppposed to a classification output)
                   trace=FALSE, #reduces amount of output printed to screen
                   maxit = maxIter, # increases max iterations to 500 from default of 100
                   MaxNWts = h*(ncol(FinalData)+1)+h+1) #says you can have one weight for each input + an additional intercept term

##calculate error measure and record results
MSE_Fit<-mean((nnetFinalFit$residuals)^2)

## Residuals and Fitted Values

Residuals <- as.vector(residuals(nnetFinalFit))
ResidualsDF <- as.data.frame(Residuals)

fittedValues<-as.vector(fitted(nnetFinalFit))
residVsFitted <- data.frame(Residuals,fittedValues)
fittedDF<-data.frame(FinalData$Time,fittedValues)
names(fittedDF)=c("Time","fittedValues")

## 4-in-1 Plots

#Normal Probability Plot
gENormProb=ggplot(ResidualsDF,aes(sample=Residuals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gEFittedVsResid = ggplot(data=residVsFitted, aes(x=fittedValues,y=Residuals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values",y = "residuals")

#Histogram of Residuals
gEResidHist = ggplot(ResidualsDF,aes(x=Residuals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEResidVsOrder =ggplot(data=ResidualsDF, aes(x=rownames(ResidualsDF), y = Residuals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


E4In1 = grid.arrange(gENormProb, gEFittedVsResid,gEResidHist, gEResidVsOrder,nrow=2,ncol=2)

#Plot Fitted Versus Actuals
gEFittedVsActuals = ggplot(data=fittedDF, aes(x=Time,y=fittedValues))+
  geom_point()+geom_line(data=RIT.train, aes(x=Time,y=NetDemand_A),color="red")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Fitted Versus Actuals", x = "Time",y = "Net Demand (kW)")
gEFittedVsActuals

#########################################
#THIS SECTION READS IN THE DATA YOU NEED TO FORECAST, TURNS IT INTO INDICATOR VARIABLES, AND NORMALIZES IT
#You choose the columns you actually need
#Use your favorite model to make a prediction for this data
#########################################
#Let's get started, by first reading in the data

# Let's prepare all your data for use, you can decide which variables to try
#Turn Day of Week, Month, Time, and Rain into Indicator Variables

#We need to revise our month of year column to include all months as factor level
EasyTest$Month<-as.factor(EasyTest$Month)###This line you only need if your data column is not already a factor
levels(EasyTest$Month)<-c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
EasyTest$Month<- factor("Feb",levels=c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep"))

EasyTest$DoM<-as.factor(EasyTest$DoM)###This line you only need if your data column is not already a factor
levels(EasyTest$DoM)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                        ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
EasyTest$DoM<- factor(c("2","3","4","5","6","7","8"),levels=c
                      ("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                        ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"))

EasyTest$Semester<-as.factor(EasyTest$Semester)###This line you only need if your data column is not already a factor
levels(EasyTest$Semester)<-c("1","2","3")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
EasyTest$Semester<- factor("2",levels=c("1","2","3"))

EasydayOfWeekIndicator<-class.ind(EasyTest$DoW)
EasymonthOfYearIndicator<-class.ind(EasyTest$Month)
EasytimeIndicator<-class.ind(EasyTest$HoD)
EasydayofMonthIndicator<-class.ind(EasyTest$DoM)
EasyRITopenIndicator<-class.ind(EasyTest$Ev_RITOpen)
EasySemesterIndicator<-class.ind(EasyTest$Semester)
EasyClassesIndicator<-class.ind(EasyTest$Ev_Classes)

#group all indicators together using only 6 days of week, 11 months, 23 hours, and 1 rain column
EasyallIndicators<-data.frame(EasydayOfWeekIndicator[,1:6],EasymonthOfYearIndicator[,1:11],EasytimeIndicator[,1:23],EasydayofMonthIndicator[,1:30],
                              EasyRITopen=EasyRITopenIndicator[,1],EasySemester=EasySemesterIndicator[,1:2],EasyClasses=EasyClassesIndicator[,1] )

#keep track of max and min bikes to denormalize later
EasyminNetDemand_A<-min(EasyTest$NetDemand_A)
EasymaxNetDemand_A<-max(EasyTest$NetDemand_A)

#normalize all the continuous columns at once by using 'sapply'
EasyallContinuous = EasyTest[,c("AW_ExtTemp","DayHighTemp","DayLowTemp","HeatReqCalc","CoolReqCalc",
                                "CW_CoolReq","CW_HeatReq","AW_FeelsLike","AW_WeatherClass","AW_ExtHum","AW_DewPoint","OA_WindSpe","OA_WindDir","NetDemand_A")]

EasynormContinuous = data.frame(sapply(EasyallContinuous, normalizefunction))

#Seeing the number of Na values
sapply(EasynormContinuous,function(x) sum(is.na(x)))
EasynormContinuous$CoolReqCalc <-0
EasynormContinuous$CW_CoolReq <-0

#create data frame that has Date, All Predictors, and Bike Traffic
EasyFinalData<-data.frame(EasyTest$Time,EasyallIndicators,EasynormContinuous)
colnames(EasyFinalData)[1] <- "Time"


#We need to revise our month of year column to include all months as factor level
DiffTest$Month<-as.factor(DiffTest$Month)###This line you only need if your data column is not already a factor
levels(DiffTest$Month)<-c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
DiffTest$Month<- factor("Dec",levels=c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep"))

DiffTest$DoM<-as.factor(DiffTest$DoM)###This line you only need if your data column is not already a factor
levels(DiffTest$DoM)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                        ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
DiffTest$DoM<- factor(c("25","26","27","28","29","30","31"),levels=c
                      ("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                        ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"))

DiffTest$Semester<-as.factor(DiffTest$Semester)###This line you only need if your data column is not already a factor
levels(DiffTest$Semester)<-c("1","2","3")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
DiffTest$Semester<- factor("1",levels=c("1","2","3"))


DiffdayOfWeekIndicator<-class.ind(DiffTest$DoW)
DiffmonthOfYearIndicator<-class.ind(DiffTest$Month)
DifftimeIndicator<-class.ind(DiffTest$HoD)
DiffdayofMonthIndicator<-class.ind(DiffTest$DoM)
DiffRITopenIndicator<-class.ind(DiffTest$Ev_RITOpen)
DiffSemesterIndicator<-class.ind(DiffTest$Semester)
DiffClassesIndicator<-class.ind(DiffTest$Ev_Classes)

#group all indicators together using only 6 days of week, 11 months, 23 hours, and 1 rain column
DiffallIndicators<-data.frame(DiffdayOfWeekIndicator[,1:6],DiffmonthOfYearIndicator[,1:11],DifftimeIndicator[,1:23],DiffdayofMonthIndicator[,1:30],
                              DiffRITopen=DiffRITopenIndicator[,1],DiffSemester=DiffSemesterIndicator[,1:2],DiffClasses=DiffClassesIndicator[,1] )

#keep track of max and min bikes to denormalize later
DiffminNetDemand_A<-min(DiffTest$NetDemand_A)
DiffmaxNetDemand_A<-max(DiffTest$NetDemand_A)

#normalize all the continuous columns at once by using 'sapply'
DiffallContinuous = DiffTest[,c("AW_ExtTemp","DayHighTemp","DayLowTemp","HeatReqCalc","CoolReqCalc",
                                "CW_CoolReq","CW_HeatReq","AW_FeelsLike","AW_WeatherClass","AW_ExtHum","AW_DewPoint","OA_WindSpe","OA_WindDir","NetDemand_A")]

DiffnormContinuous = data.frame(sapply(DiffallContinuous, normalizefunction))

#Seeing the number of Na values
sapply(DiffnormContinuous,function(x) sum(is.na(x)))
DiffnormContinuous$CoolReqCalc <-0
DiffnormContinuous$CW_CoolReq <-0

#create data frame that has Date, All Predictors, and Bike Traffic
DiffFinalData<-data.frame(DiffTest$Time,DiffallIndicators,DiffnormContinuous)
colnames(DiffFinalData)[1] <- "Time"



#We need to revise our month of year column to include all months as factor level
MarchLastWeek$Month<-as.factor(MarchLastWeek$Month)###This line you only need if your data column is not already a factor
levels(MarchLastWeek$Month)<-c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
MarchLastWeek$Month<- factor("Mar",levels=c("Apr","Aug","Dec","Jan","Feb","Jul","Jun","Mar","May","Nov","Oct","Sep"))

MarchLastWeek$DoM<-as.factor(MarchLastWeek$DoM)###This line you only need if your data column is not already a factor
levels(MarchLastWeek$DoM)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                             ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
MarchLastWeek$DoM<- factor(c("25","26","27","28","29","30","31"),levels=c
                           ("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"
                             ,"16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"))

MarchLastWeek$Semester<-as.factor(MarchLastWeek$Semester)###This line you only need if your data column is not already a factor
levels(MarchLastWeek$Semester)<-c("1","2","3")
#for some reason the above line for levels changes the Month to Apr which is bizarre, this line puts it back to May
MarchLastWeek$Semester<- factor("2",levels=c("1","2","3"))


LMardayOfWeekIndicator<-class.ind(MarchLastWeek$DoW)
LMarmonthOfYearIndicator<-class.ind(MarchLastWeek$Month)
LMartimeIndicator<-class.ind(MarchLastWeek$HoD)
LMardayofMonthIndicator<-class.ind(MarchLastWeek$DoM)
LMarRITopenIndicator<-class.ind(MarchLastWeek$Ev_RITOpen)
LMarSemesterIndicator<-class.ind(MarchLastWeek$Semester)
LMarClassesIndicator<-class.ind(MarchLastWeek$Ev_Classes)

#group all indicators together using only 6 days of week, 11 months, 23 hours, and 1 rain column
LMarallIndicators<-data.frame(LMardayOfWeekIndicator[,1:6],LMarmonthOfYearIndicator[,1:11],LMartimeIndicator[,1:23],LMardayofMonthIndicator[,1:30],
                              LMarRITopen=DiffRITopenIndicator[,1],LMarSemester=DiffSemesterIndicator[,1:2],LMarClasses=LMarClassesIndicator[,1] )

#keep track of max and min Net Demand to denormalize later
#Since we do not have the actual values for March, I used the last week of March in 2021 and calculate the max and min based on this week
LAstWeekMarch2021 <- AllData[AllData$Time>="2021-03-25 00:00:00"& AllData$Time<"2021-04-01 00:00:00",]

LMarminNetDemand_A<-min(LAstWeekMarch2021$NetDemand_A)
LMarmaxNetDemand_A<-max(LAstWeekMarch2021$NetDemand_A)

#normalize all the continuous columns at once by using 'sapply'
LMarallContinuous = MarchLastWeek[,c("AW_ExtTemp","DayHighTemp","DayLowTemp","HeatReqCalc","CoolReqCalc",
                                     "CW_CoolReq","CW_HeatReq","AW_FeelsLike","AW_WeatherClass","AW_ExtHum","AW_DewPoint","OA_WindSpe","OA_WindDir")]

LMarnormContinuous = data.frame(sapply(LMarallContinuous, normalizefunction))

#Seeing the number of Na values
sapply(LMarnormContinuous,function(x) sum(is.na(x)))
LMarnormContinuous$CoolReqCalc <-0
LMarnormContinuous$CW_CoolReq <-0

#create data frame that has Date, All Predictors, and Bike Traffic
LMarFinalData<-data.frame(MarchLastWeek$Time,LMarallIndicators,LMarnormContinuous)
colnames(LMarFinalData)[1] <- "Time"


###########################################################################################
###########################Forecast for Each Test Week#####################################

#Make predictions for the test week using your best model (BE SURE TO SPECIFY THE CORRECT COLUMNS)
EasytestWeekPredictions <-predict(nnetFinalFit,EasyFinalData[,c(2:75,81,82,85,86,87,88)])


Easyforecasts <-data.frame(Date=EasyFinalData$Time,EasytestWeekPredictions,EasyActuals=EasyTest$NetDemand_A)
Easyforecasts$Date= as.POSIXct(Easyforecasts$Date,tz="","%m/%d/%Y %H:%M")

Easyg = ggplot(EasyTest, aes(x=Time,y=NetDemand_A))+
  geom_line(size= 1 ,na.rm = TRUE)+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(x = "Time", y = "Demand (kW)", title = "Time Series Plot for buildingA Electric Demand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%Y"),breaks = date_breaks("1 day")) 
Easyg

#plot(Easyforecasts,main="Forecasts for Easy Test Week",ylab="Norm Net Demand A")

EasytestWeekPredictionsUnnorm<-unnormalizefunction(x=EasytestWeekPredictions,min=EasyminNetDemand_A,max=EasymaxNetDemand_A)
EasyforecastOriginalScale<-data.frame(Date=EasyFinalData$Time,EasytestWeekPredictionsUnnorm,EasyActuals=EasyTest$NetDemand_A)
#plot(EasyforecastOriginalScale,main="Forecasts for Easy Test Week",type="l",ylab="Net Demand A")

mapeEasyTest=mean(abs(EasyforecastOriginalScale$EasytestWeekPredictionsUnnorm - EasyTest$NetDemand_A)/EasyTest$NetDemand_A)*100
mapeEasyTest = round(mapeEasyTest, digits = 3)

#create a string for the graph title
titleString = paste("Easy Week Set Actuals Vs Forecasts")

colors <- c("Actuals" = "black","Neural Networks Forecast"= "red")

EZoomedTEstForecast <- ggplot(EasyforecastOriginalScale,aes(x=Date,y=EasytestWeekPredictionsUnnorm, color="Neural Networks Forecast"))+
  labs(y="Net Demand", x="Time",title="Forecast for the Easy Test Set- Neural Networks")+
  geom_line(size = 1)+geom_point(data=EasyTest,aes(x=Time,y=NetDemand_A, color="Actuals"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors)+labs(colour="Legend")+
  annotate(geom="text",x=EasyTest[120,1],y=5000,label=paste("MAPE=",mapeEasyTest))
EZoomedTEstForecast



## Residuals and Fitted Values

EasyResiduals <- as.vector(EasyTest$NetDemand_A - EasyforecastOriginalScale$EasytestWeekPredictionsUnnorm)
EasyResidualsDF <- as.data.frame(EasyResiduals)

#fittedValues<-as.vector(fitted(DifferentnnetFit))
EasyresidVsFitted <- data.frame(EasyResiduals,EasyfittedValues=EasyforecastOriginalScale$EasytestWeekPredictionsUnnorm)
EasyfittedDF<-data.frame(Date=EasyFinalData$Time,EasytestWeekPredictionsUnnorm)
names(EasyfittedDF)=c("Time","fittedValues")

## 4-in-1 Plots

#Normal Probability Plot
gENormProb=ggplot(EasyResidualsDF,aes(sample=EasyResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gEFittedVsResid = ggplot(data=EasyresidVsFitted, aes(x=EasyfittedValues,y=EasyResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Forecasted Values", x = "Forecasted Values",y = "residuals")

#Histogram of Residuals
gEResidHist = ggplot(EasyResidualsDF,aes(x=EasyResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEResidVsOrder =ggplot(data=EasyResidualsDF, aes(x=rownames(EasyResidualsDF), y = EasyResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


E4In1 = grid.arrange(gENormProb, gEFittedVsResid,gEResidHist, gEResidVsOrder,nrow=2,ncol=2)




##########################################Diff#################################################################
DifftestWeekPredictions <-predict(nnetFinalFit,DiffFinalData[,c(2:75,81,82,85,86,87,88)])


Diffforecasts <-data.frame(Date=DiffFinalData$Time,DifftestWeekPredictions)

plot(Diffforecasts,main="Forecasts for Difficult Test Week", type="l",ylab="Norm Net Demand A")

DifftestWeekPredictionsUnnorm<-unnormalizefunction(x=DifftestWeekPredictions,min=DiffminNetDemand_A,max=DiffmaxNetDemand_A)
DiffforecastOriginalScale<-data.frame(Date=DiffFinalData$Time,DifftestWeekPredictionsUnnorm)
plot(DiffforecastOriginalScale,main="Forecasts for Difficult Test Week",type="l",ylab="Net Demand A")

mapeDiffTest=mean(abs(DiffforecastOriginalScale$DifftestWeekPredictionsUnnorm - DiffTest$NetDemand_A)/DiffTest$NetDemand_A)*100
mapeDiffTest = round(mapeDiffTest, digits = 3)


#create a string for the graph title
titleString = paste("Difficult Week Set Actuals Vs Forecasts")

colors <- c("Actuals" = "black","Neural Networks Forecast"= "red")

DiffZoomedTEstForecast <- ggplot(DiffforecastOriginalScale,aes(x=Date,y=DifftestWeekPredictionsUnnorm, color="Neural Networks Forecast"))+
  labs(y="Net Demand", x="Time",title="Forecast for the Difficult Test Set- Neural Networks")+
  geom_line(size = 1)+geom_point(data=DiffTest,aes(x=Time,y=NetDemand_A, color="Actuals"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_x_datetime(date_breaks="1 day",date_labels = "%d-%b-%y")+
  labs(title = titleString, x = "Time",y = "Net Demand (Kw)")+
  scale_colour_manual(values= colors)+labs(colour="Legend")+
  annotate(geom="text",x=DiffTest[50,1],y=3500,label=paste("MAPE=",mapeDiffTest))
DiffZoomedTEstForecast


## Residuals and Fitted Values

DiffResiduals <- as.vector(DiffTest$NetDemand_A - DiffforecastOriginalScale$DifftestWeekPredictionsUnnorm)
DiffResidualsDF <- as.data.frame(DiffResiduals)

#fittedValues<-as.vector(fitted(DifferentnnetFit))
DiffresidVsFitted <- data.frame(DiffResiduals,DifffittedValues=DiffforecastOriginalScale$DifftestWeekPredictionsUnnorm)
DifffittedDF<-data.frame(Date=DiffFinalData$Time,DifftestWeekPredictionsUnnorm)
names(DifffittedDF)=c("Time","fittedValues")

## 4-in-1 Plots

#Normal Probability Plot
gENormProb=ggplot(DiffResidualsDF,aes(sample=DiffResiduals))+stat_qq()+stat_qq_line()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Normal Probability Plot")

#Fitted Vs Residuals
gEFittedVsResid = ggplot(data=DiffresidVsFitted, aes(x=DifffittedValues,y=DiffResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
  labs(title = "Residuals vs Forecasted Values", x = "Forecasted Values",y = "residuals")

#Histogram of Residuals
gEResidHist = ggplot(DiffResidualsDF,aes(x=DiffResiduals))+geom_histogram(color="black",fill="white")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  labs(title = "Histogram of Residuals", x = "Residuals",y = "Frequency")

#Residuals Versus Observation Order
gEResidVsOrder =ggplot(data=DiffResidualsDF, aes(x=rownames(DiffResidualsDF), y = DiffResiduals))+
  geom_point()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_blank())+ 
  labs(title = "Residuals vs Observation Order", x = "Order",y = "Residuals")


E4In1 = grid.arrange(gENormProb, gEFittedVsResid,gEResidHist, gEResidVsOrder,nrow=2,ncol=2)




#####################################Last Week of March Forecast################################################


LMarWeekPredictions <-predict(nnetFinalFit,LMarFinalData[,c(2:75,81,82,85,86,87,88)])


LMarforecasts <-data.frame(Date=LMarFinalData$Time,LMarWeekPredictions)

plot(LMarforecasts,main="Forecasts for Last Week of March", type="l",ylab="Norm Net Demand A")

LMarWeekPredictionsUnnorm<-unnormalizefunction(x=LMarWeekPredictions,min=LMarminNetDemand_A,max=LMarmaxNetDemand_A)
LMarforecastOriginalScale<-data.frame(Date=LMarFinalData$Time,LMarWeekPredictionsUnnorm)
plot(LMarforecastOriginalScale,main="Forecasts for Last Week of March",type="l",ylab="Net Demand A")

#Look at your forecast, does it seem reasonable?

write.csv(LMarforecastOriginalScale,file="Last Week of March Forecast.csv")