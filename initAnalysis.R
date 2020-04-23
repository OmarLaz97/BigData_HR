# clean environment 
rm(list=ls())

#install.packages("gridExtra")
#install.packages("ggcorrplot")
#install.packages("corrplot")

# imports
library(ggplot2)
library(scales)
library(gridExtra)
library(corrplot)

#Importing data from csv files and reading data into a data frame
data <- read.csv("employee_data.csv")
data <- as.data.frame(data)
#get dimensions of data frame
dim(data)
#Structure of the data frame
structure(data)
#Get the first and last 10 rows
head(data,10)
tail(data,10)
#Show summary of the dataframe
summary(data)

# Data with attrition is Yes and No
dataNo <- data[data$Attrition == "No",]
dataYes <- data[data$Attrition == "Yes",]

# Simple visualization 
# Attrition Rate
Attrition <- ggplot(data = data, aes(Attrition)) 
Attrition <- Attrition + geom_bar(fill = "steelblue", color ="steelblue") 
Attrition
# Business Travel
BTravel <- ggplot(data = data, aes(BusinessTravel))
BTravel <- BTravel + geom_bar(fill = "steelblue", color ="steelblue") 
BTravel
# Department 
Department <- ggplot(data = data, aes(Department))
Department <- Department + geom_bar(fill = "steelblue", color ="steelblue") 
Department
# Distance From Home
DistFromHome <- ggplot(data = data, aes(x = DistanceFromHome))
DistFromHome <- DistFromHome + geom_freqpoly(fill = "steelblue", color ="steelblue")
DistFromHome
# Education
Education <- ggplot(data = data, aes(Education))
Education <- Education + geom_bar(fill = "steelblue", color ="steelblue") 
Education
# Education Field
EducationField <- ggplot(data = data, aes(EducationField))
EducationField <- EducationField + geom_bar(fill = "steelblue", color ="steelblue") 
EducationField
# Gender 
Gender <- ggplot(data = data, aes(Gender))
Gender <- Gender + geom_bar(fill = "steelblue", color ="steelblue") 
Gender
# JobLevel
JobLevel <- ggplot(data = data, aes(JobLevel))
JobLevel <- JobLevel + geom_bar(fill = "steelblue", color ="steelblue") 
JobLevel
# JobRole
JobRole <- ggplot(data = data, aes(JobRole))
JobRole <- JobRole + geom_bar(fill = "steelblue", color ="steelblue") 
JobRole
# MaritalStatus
MaritalStatus <- ggplot(data = data, aes(MaritalStatus))
MaritalStatus <- MaritalStatus + geom_bar(fill = "steelblue", color ="steelblue") 
MaritalStatus
# MonthlyIncome
MonthlyIncome <- ggplot(data = data, aes(MonthlyIncome))
MonthlyIncome <- MonthlyIncome + geom_density(fill = "steelblue", color ="steelblue")
MonthlyIncome
# NumCompaniesWorked
NumCompaniesWorked <- ggplot(data = data, aes(NumCompaniesWorked))
NumCompaniesWorked <- NumCompaniesWorked + geom_bar(fill = "steelblue", color ="steelblue") 
NumCompaniesWorked
# PercentSalaryHike
PercentSalaryHike <- ggplot(data = data, aes(PercentSalaryHike))
PercentSalaryHike <- PercentSalaryHike + geom_density(fill = "steelblue", color ="steelblue") 
PercentSalaryHike
# StockOptionLevel
StockOptionLevel <- ggplot(data = data, aes(StockOptionLevel))
StockOptionLevel <- StockOptionLevel + geom_bar(fill = "steelblue", color ="steelblue") 
StockOptionLevel
# TotalWorkingYears
TotalWorkingYears <- ggplot(data = data, aes(TotalWorkingYears))
TotalWorkingYears <- TotalWorkingYears + geom_bar(fill = "steelblue", color ="steelblue") 
TotalWorkingYears
# YearsAtCompany
YearsAtCompany <- ggplot(data = data, aes(YearsAtCompany))
YearsAtCompany <- YearsAtCompany + geom_bar(fill = "steelblue", color ="steelblue") 
YearsAtCompany
# YearsSinceLastPromotion
YearsSinceLastPromotion <- ggplot(data = data, aes(YearsSinceLastPromotion))
YearsSinceLastPromotion <- YearsSinceLastPromotion + geom_density(fill = "steelblue", color ="steelblue") 
YearsSinceLastPromotion
# YearsWithCurrManager
YearsWithCurrManager <- ggplot(data = data, aes(YearsWithCurrManager))
YearsWithCurrManager <- YearsWithCurrManager + geom_bar(fill = "steelblue", color ="steelblue") 
YearsWithCurrManager
# EnvironmentSatisfaction
EnvironmentSatisfaction <- ggplot(data = data, aes(EnvironmentSatisfaction))
EnvironmentSatisfaction <- EnvironmentSatisfaction + geom_bar(fill = "steelblue", color ="steelblue") 
EnvironmentSatisfaction
# JobSatisfaction
JobSatisfaction <- ggplot(data = data, aes(JobSatisfaction))
JobSatisfaction <- JobSatisfaction + geom_bar(fill = "steelblue", color ="steelblue") 
JobSatisfaction
# WorkLifeBalance
WorkLifeBalance <- ggplot(data = data, aes(WorkLifeBalance))
WorkLifeBalance <- WorkLifeBalance + geom_bar(fill = "steelblue", color ="steelblue") 
WorkLifeBalance
# JobInvolvement
JobInvolvement <- ggplot(data = data, aes(JobInvolvement))
JobInvolvement <- JobInvolvement + geom_bar(fill = "steelblue", color ="steelblue") 
JobInvolvement
# PerformanceRating
PerformanceRating <- ggplot(data = data, aes(PerformanceRating))
PerformanceRating <- PerformanceRating + geom_bar(fill = "steelblue", color ="steelblue") 
PerformanceRating
# AverageWorkingHours
AverageWorkingHours <- ggplot(data = data, aes(AverageWorkingHours))
AverageWorkingHours <- AverageWorkingHours + geom_density(fill = "steelblue", color ="steelblue") 
AverageWorkingHours


######################## Attrition VS Categorical Data ########################
# Attrition Vs Gender
AttVGender <- ggplot(data = data, aes(Attrition)) + geom_bar(aes(fill=Gender), position = "fill") 
AttVGender
# Attrition Vs BusinessTravel
TravVAttr <- ggplot(data = data, aes(fill=Attrition, BusinessTravel)) + geom_bar(position="fill") 
TravVAttr
# Attrition Vs WorkLifeBalance
BalanceVAttr <- ggplot(data = data, aes(fill=Attrition, WorkLifeBalance)) + geom_bar(position="fill") 
BalanceVAttr
# Attrition Vs Education
AttrVEdu <- ggplot(data = data, aes(Education, fill=Attrition)) + geom_bar(position = "fill")
AttrVEdu
# Attrition Vs Education Field
AttrVEdu <- ggplot(data = data, aes(EducationField, fill=Attrition)) + geom_bar(position = "fill") 
AttrVEdu
# Attrition Vs JobLevel
AttrVJL <- ggplot(data = data, aes(JobLevel, fill=Attrition)) + geom_bar(position = "fill")
AttrVJL
# Attrition Vs MaritalStatus
AttrVStatus <- ggplot(data = data, aes(MaritalStatus, fill=Attrition)) + geom_bar(position = "fill")
AttrVStatus
# Attrition vs Dep
AttVDep <- ggplot(data = data, aes(Department, fill = Attrition)) + geom_bar(position = "fill")
AttVDep
# Attrition vs JobRole
AttVJR <- ggplot(data = data, aes(JobRole, fill = Attrition)) + geom_bar(position = "fill")
AttVJR
# Attrition vs Env Satisfaction
AttVEnvSat <- ggplot(data = data, aes(EnvironmentSatisfaction, fill = Attrition)) + geom_bar(position = "fill")
AttVEnvSat
# Attrition vs Job satisfaction
AttVJobSat <- ggplot(data = data, aes(JobSatisfaction, fill = Attrition)) + geom_bar(position = "fill")
AttVJobSat
######################## END of Attrition VS Categorical Data ########################

######################## Attrition VS Continous Data ########################

# Attrition VS Age
AttVAge <- ggplot(data=data, aes(x = Age)) + geom_bar(aes(fill = Attrition)) 
AttVAge
# Attrition VS DistanceFromHome
AttrVDist <- ggplot(data = data, aes(DistanceFromHome)) + geom_bar(aes(fill = Attrition), position = "fill")
AttrVDist
# Attrition VS Salary hike 
AttVHike <- ggplot(data = data, aes(x = Attrition, y = data$PercentSalaryHike)) + geom_boxplot()
AttVHike
# Attrition VS (num of companies / totalyears)
rateOfAttrition <- data$NumCompaniesWorked/data$TotalWorkingYears
data$rateOfAttrition <- c(rateOfAttrition)
AttrVRate <- ggplot(data = data, aes(x = Attrition ,y=rateOfAttrition)) + geom_boxplot()
AttrVRate
# Attrition vs Training times last year
AttVTrain <- ggplot(data = data, aes(TrainingTimesLastYear, fill = Attrition)) + geom_bar(position = "fill")
AttVTrain
# Attrition vs TotalWorkingHours
AttVWorkingHours <- ggplot(data = data, aes(AverageWorkingHours, color = Attrition)) + geom_density()
AttVWorkingHours
# Attrition VS Years With current manager
AttVCurrManager <- ggplot(data = data, aes(YearsWithCurrManager, fill = Attrition)) + geom_bar(position = "fill")
AttVCurrManager

######################## END of Attrition VS Continous Data ########################

######################## Experimental Insights ########################
# Age vs Gender
AgeVGender <- ggplot(data = data, aes(x = Gender, y = Age)) + geom_boxplot(aes(fill = Gender))
AgeVGender
# BusinessTravel Vs Age & Gender
TravelVAgeAndGender <- ggplot(data = data, aes(x = BusinessTravel, y = Age)) + geom_boxplot(aes(fill = Gender))
TravelVAgeAndGender
# DistanceFromHome and gender
DistVGender <- ggplot(data = data, aes(DistanceFromHome)) + geom_bar(aes(fill = Gender), position="fill")
DistVGender
# Attrition Vs DistanceFromHome, Gender
AttVDistGender <- ggplot(data = data, aes(x = Attrition, y = DistanceFromHome)) + geom_boxplot(aes(fill = Gender))
AttVDistGender
# Attrition Vs Age & Gender
AttVAgeAndGender <- ggplot(data = data, aes(x = Attrition, y = Age)) + geom_boxplot(aes(fill = Gender))
AttVAgeAndGender
# Monthly salary vs education field
SalaryVEducationField <- ggplot(data = data, aes(x = EducationField ,y=MonthlyIncome)) + geom_boxplot()
SalaryVEducationField
# Monthly salary vs performance
data$PerformanceRating <- as.factor(data$PerformanceRating)
SalaryVPerf <- ggplot(data = data, aes(x = PerformanceRating ,y = MonthlyIncome)) + geom_boxplot()
SalaryVPerf
# Marital Status Vs Working Hours
StatusVHours <- ggplot(data = data, aes(x = MaritalStatus ,y = AverageWorkingHours)) + geom_boxplot()
StatusVHours
# Business Travel VS Marital satuts Vs Attrition
BusiVStatusNO <- ggplot(data = dataNo, aes(BusinessTravel)) + geom_bar(aes(fill = MaritalStatus), position = "fill")
BusiVStatusYes <- ggplot(data = dataYes, aes(BusinessTravel)) + geom_bar(aes(fill = MaritalStatus), position = "fill")
grid.arrange(BusiVStatusNO, BusiVStatusYes, ncol=2)
# job Role and dep
JRVDepNO <- ggplot(data = dataNo, aes(JobRole, fill = Department)) + geom_bar(position = "fill")
JRVDepYes <- ggplot(data = dataYes, aes(JobRole, fill = Department)) + geom_bar(position = "fill")
grid.arrange(JRVDepNO, JRVDepYes, ncol=2)
# Job satisfaction with job role and department
JRVDepVSatis <- ggplot(data = data, aes(x=interaction(JobRole, JobSatisfaction), fill = Department ))
JRVDepVSatis <- JRVDepVSatis + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
JRVDepVSatis
#JrVAttrVEdField
JRVAttrVEdField <- ggplot(data = data, aes(x=interaction(EducationField, JobRole), fill = Attrition ))
JRVAttrVEdField <- JRVAttrVEdField + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
JRVAttrVEdField
# Education field vs dep
EducFieldVDep <- ggplot(data = data, aes(EducationField, fill = Department))
EducFieldVDep <- EducFieldVDep + geom_bar(position = "fill")
EducFieldVDep
######################## END of Experimental Insights ########################


