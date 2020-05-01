rm(list=ls())
library(lubridate)
#Set the working directory to the folder that contains the dataset files
inTime <- read.table("in_time.csv", header=TRUE, sep=",")
outTime <- read.table("out_time.csv", header=TRUE, sep=",")
generalData <- read.table("general_data.csv", header=TRUE, sep=",")
employeeSurvey <- read.table("employee_survey_data.csv", header=TRUE, sep=",")
managerSurvey <- read.table("manager_survey_data.csv", header=TRUE, sep=",")

#average working hours per day for each employee
inTime<-inTime[-c(1,2)]
outTime<-outTime[-c(1,2)]
timediff <- function(u,v) as.numeric(difftime(ymd_hms(u),ymd_hms(v),units=c("hours")), units="hours")
hours<-mapply(timediff, outTime, inTime)
hours<-as.data.frame(hours)
hours[is.na(hours)] <- 0
avgHrs<-rowMeans(hours)

#removing unnecessary columns
generalData<-generalData[ , !(names(generalData) %in% c("Over18", "StandardHours","EmployeeCount"))]

#Merge tables
employeeData<-merge(generalData,employeeSurvey,by="EmployeeID")
employeeData<-merge(employeeData,managerSurvey, by="EmployeeID")
#Add average working hours
employeeData$AverageWorkingHours<-avgHrs
#Remove NAs
employeeData<-employeeData[complete.cases(employeeData), ]
#Export to csv file
write.csv(employeeData,"employee_data.csv")
