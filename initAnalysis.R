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
ggsave("./assets/attrition.png", width = 26, height = 28, units = "cm", device = NULL)

# Age 
Age <- ggplot(data = data, aes(Age))
Age <- Age + geom_density(fill = "steelblue", color ="steelblue") 
Age
ggsave("./assets/Age.png", width = 26, height = 28, units = "cm", device = NULL)

# Business Travel
BTravel <- ggplot(data = data, aes(BusinessTravel))
BTravel <- BTravel + geom_bar(fill = "steelblue", color ="steelblue") 
BTravel
ggsave("./assets/BTravel.png", width = 26, height = 28, units = "cm", device = NULL)

# Department 
Department <- ggplot(data = data, aes(Department))
Department <- Department + geom_bar(fill = "steelblue", color ="steelblue") 
Department
ggsave("./assets/department.png", width = 26, height = 28, units = "cm", device = NULL)

# Distance From Home
DistFromHome <- ggplot(data = data, aes(x = DistanceFromHome))
DistFromHome <- DistFromHome + geom_density(fill = "steelblue", color ="steelblue")
DistFromHome + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/distfromHome.png", width = 26, height = 28, units = "cm", device = NULL)

# Education
Education <- ggplot(data = data, aes(Education))
Education <- Education + geom_bar(fill = "steelblue", color ="steelblue") 
Education
ggsave("./assets/education.png", width = 26, height = 28, units = "cm", device = NULL)

# Education Field
EducationField <- ggplot(data = data, aes(EducationField))
EducationField <- EducationField + geom_bar(fill = "steelblue", color ="steelblue") 
EducationField
ggsave("./assets/educationField.png", width = 26, height = 28, units = "cm", device = NULL)

# Gender 
Gender <- ggplot(data = data, aes(Gender))
Gender <- Gender + geom_bar(fill = "steelblue", color ="steelblue") 
Gender
ggsave("./assets/Gender.png", width = 26, height = 28, units = "cm", device = NULL)

# JobLevel
JobLevel <- ggplot(data = data, aes(JobLevel))
JobLevel <- JobLevel + geom_bar(fill = "steelblue", color ="steelblue") 
JobLevel
ggsave("./assets/jobLevel.png", width = 26, height = 28, units = "cm", device = NULL)

# JobRole
JobRole <- ggplot(data = data, aes(JobRole))
JobRole <- JobRole + geom_bar(fill = "steelblue", color ="steelblue") 
JobRole + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("./assets/jobRole.png", width = 26, height = 28, units = "cm", device = NULL)

# MaritalStatus
MaritalStatus <- ggplot(data = data, aes(MaritalStatus))
MaritalStatus <- MaritalStatus + geom_bar(fill = "steelblue", color ="steelblue") 
MaritalStatus
ggsave("./assets/MaritalStatus.png", width = 26, height = 28, units = "cm", device = NULL)

# MonthlyIncome
MonthlyIncome <- ggplot(data = data, aes(MonthlyIncome))
MonthlyIncome <- MonthlyIncome + geom_density(fill = "steelblue", color ="steelblue")
MonthlyIncome + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/MonthlyIncome.png", width = 26, height = 28, units = "cm", device = NULL)

# NumCompaniesWorked
NumCompaniesWorked <- ggplot(data = data, aes(NumCompaniesWorked))
NumCompaniesWorked <- NumCompaniesWorked + geom_bar(fill = "steelblue", color ="steelblue") 
NumCompaniesWorked
ggsave("./assets/NumCompaniesWorked.png", width = 26, height = 28, units = "cm", device = NULL)

# PercentSalaryHike
PercentSalaryHike <- ggplot(data = data, aes(PercentSalaryHike))
PercentSalaryHike <- PercentSalaryHike + geom_density(fill = "steelblue", color ="steelblue") 
PercentSalaryHike + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
# StockOptionLevel
StockOptionLevel <- ggplot(data = data, aes(StockOptionLevel))
StockOptionLevel <- StockOptionLevel + geom_bar(fill = "steelblue", color ="steelblue") 
StockOptionLevel
# TotalWorkingYears
TotalWorkingYears <- ggplot(data = data, aes(TotalWorkingYears))
TotalWorkingYears <- TotalWorkingYears + geom_density(fill = "steelblue", color ="steelblue") 
TotalWorkingYears + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/TotalWorkingYears.png", width = 26, height = 28, units = "cm", device = NULL)

# YearsAtCompany
YearsAtCompany <- ggplot(data = data, aes(YearsAtCompany))
YearsAtCompany <- YearsAtCompany + geom_density(fill = "steelblue", color ="steelblue") 
YearsAtCompany + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/yearsAtCompany.png", width = 26, height = 28, units = "cm", device = NULL)

# YearsSinceLastPromotion
YearsSinceLastPromotion <- ggplot(data = data, aes(YearsSinceLastPromotion))
YearsSinceLastPromotion <- YearsSinceLastPromotion + geom_density(fill = "steelblue", color ="steelblue") 
YearsSinceLastPromotion + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/sincelastPromo.png", width = 26, height = 28, units = "cm", device = NULL)

# YearsWithCurrManager
YearsWithCurrManager <- ggplot(data = data, aes(YearsWithCurrManager))
YearsWithCurrManager <- YearsWithCurrManager + geom_bar(fill = "steelblue", color ="steelblue") 
YearsWithCurrManager

# EnvironmentSatisfaction
EnvironmentSatisfaction <- ggplot(data = data, aes(EnvironmentSatisfaction))
EnvironmentSatisfaction <- EnvironmentSatisfaction + geom_bar(fill = "steelblue", color ="steelblue") 
EnvironmentSatisfaction
ggsave("./assets/envSatisfaction.png", width = 26, height = 28, units = "cm", device = NULL)

# JobSatisfaction
JobSatisfaction <- ggplot(data = data, aes(JobSatisfaction))
JobSatisfaction <- JobSatisfaction + geom_bar(fill = "steelblue", color ="steelblue") 
JobSatisfaction
ggsave("./assets/jobsatist.png", width = 26, height = 28, units = "cm", device = NULL)

# WorkLifeBalance
WorkLifeBalance <- ggplot(data = data, aes(WorkLifeBalance))
WorkLifeBalance <- WorkLifeBalance + geom_bar(fill = "steelblue", color ="steelblue") 
WorkLifeBalance
ggsave("./assets/worklifebalance.png", width = 26, height = 28, units = "cm", device = NULL)

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
AverageWorkingHours  + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/avgWorkingHours.png", width = 26, height = 28, units = "cm", device = NULL)



######################## Attrition VS Categorical Data ########################
# Attrition Vs Gender
AttVGender <- ggplot(data = data, aes(fill=Attrition, Gender)) + geom_bar(position="fill") 
AttVGender
# Attrition Vs BusinessTravel
TravVAttr <- ggplot(data = data, aes(fill=Attrition, BusinessTravel)) + geom_bar(position="fill") 
TravVAttr
ggsave("./assets/AttrvsBT.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition Vs WorkLifeBalance
BalanceVAttr <- ggplot(data = data, aes(fill=Attrition, WorkLifeBalance)) + geom_bar(position="fill") 
BalanceVAttr
ggsave("./assets/worklifebalanceVsattr.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition Vs Education
AttrVEdu <- ggplot(data = data, aes(Education, fill=Attrition)) + geom_bar(position = "fill")
AttrVEdu
# Attrition Vs Education Field
AttrVEdu <- ggplot(data = data, aes(EducationField, fill=Attrition)) + geom_bar(position = "fill") 
AttrVEdu
ggsave("./assets/EducFieldvsattr.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition Vs JobLevel
AttrVJL <- ggplot(data = data, aes(JobLevel, fill=Attrition)) + geom_bar(position = "fill")
AttrVJL
# Attrition Vs MaritalStatus
AttrVStatus <- ggplot(data = data, aes(MaritalStatus, fill=Attrition)) + geom_bar(position = "fill")
AttrVStatus
ggsave("./assets/attrvsMaritalStatus.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs Dep
AttVDep <- ggplot(data = data, aes(Department, fill = Attrition)) + geom_bar(position = "fill")
AttVDep
ggsave("./assets/attrVSdep.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs JobRole
AttVJR <- ggplot(data = data, aes(JobRole, fill = Attrition)) + geom_bar(position = "fill")
AttVJR
ggsave("./assets/attrvsJobrole.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs Env Satisfaction
AttVEnvSat <- ggplot(data = data, aes(EnvironmentSatisfaction, fill = Attrition)) + geom_bar(position = "fill")
AttVEnvSat
ggsave("./assets/attrvsenvsat.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs Job satisfaction
AttVJobSat <- ggplot(data = data, aes(JobSatisfaction, fill = Attrition)) + geom_bar(position = "fill")
AttVJobSat
ggsave("./assets/attrvsjobsat.png", width = 26, height = 28, units = "cm", device = NULL)

######################## END of Attrition VS Categorical Data ########################

######################## Attrition VS Continous Data ########################

# Attrition VS Age
AttVAge <- ggplot(data=data, aes(y = Age, x = Attrition)) + geom_boxplot(aes(color = Attrition)) 
AttVAge + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggsave("./assets/attrvsage.png", width = 26, height = 28, units = "cm", device = NULL)

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

AttrVRate <- ggplot(data = data, aes(x = Attrition ,y=TotalWorkingYears, color = Attrition)) + geom_boxplot()
AttrVRate
ggsave("./assets/attrvstotalworkingyears.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs Training times last year
AttVTrain <- ggplot(data = data, aes(y = TrainingTimesLastYear,x = Attrition, color = Attrition)) + geom_boxplot()
AttVTrain
# Attrition vs TotalWorkingHours
AttVWorkingHours <- ggplot(data = data, aes(y = AverageWorkingHours,x = Attrition, color = Attrition)) + geom_boxplot()
AttVWorkingHours
ggsave("./assets/attrvstotalhours.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition VS Years With current manager
AttVCurrManager <- ggplot(data = data, aes(y = YearsWithCurrManager, x = Attrition,  color = Attrition)) + geom_boxplot()
AttVCurrManager
ggsave("./assets/attrvsYearsWithcurr.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition vs years at company
AttVYearsAtCompany <- ggplot(data = data, aes(y = YearsAtCompany, x = Attrition,  color = Attrition)) + geom_boxplot()
AttVYearsAtCompany
ggsave("./assets/attrvsYearsatcompany.png", width = 26, height = 28, units = "cm", device = NULL)


######################## END of Attrition VS Continous Data ########################

######################## Experimental Insights ########################
# Age vs Gender
AgeVGender <- ggplot(data = data, aes(y=Age, x=Gender)) + geom_boxplot(aes(fill=Gender))
AgeVGender
ggsave("./assets/attrvsgenderandage.png", width = 26, height = 28, units = "cm", device = NULL)

# BusinessTravel Vs Age & Gender
TravelVAgeAndGender <- ggplot(data = data, aes(x = BusinessTravel, y = Age)) + geom_boxplot(aes(fill = Gender))
TravelVAgeAndGender
# DistanceFromHome and gender
DistVGender <- ggplot(data = data, aes(y=DistanceFromHome, x=Gender)) + geom_boxplot(aes(fill = Gender))
DistVGender
ggsave("./assets/attrvsgenderanddist.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition Vs DistanceFromHome, Gender
data$Attrition <- as.factor(data$Attrition)
AttVDistGender <- ggplot(data = data, aes(x=Attrition, y=DistanceFromHome)) + geom_boxplot(aes(fill = Gender))
AttVDistGender
ggsave("./assets/attrvsdistandgender.png", width = 26, height = 28, units = "cm", device = NULL)

# Attrition Vs Age & Gender
AttVAgeAndGender <- ggplot(data = data, aes(x = Attrition, y = Age)) + geom_boxplot(aes(fill = Gender))
AttVAgeAndGender
ggsave("./assets/attrvsageandgender.png", width = 26, height = 28, units = "cm", device = NULL)

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

# Education field vs dep
EducFieldVDep <- ggplot(data = data, aes(EducationField, fill = Department))
EducFieldVDep <- EducFieldVDep + geom_bar(position = "fill")
EducFieldVDep

dataYes$JobSatisfaction <- as.factor(dataYes$JobSatisfaction)
dataYes$EnvironmentSatisfaction <- as.factor(dataYes$EnvironmentSatisfaction)
dataYes$WorkLifeBalance <- as.factor(dataYes$WorkLifeBalance)

data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)

dataNo$JobSatisfaction <- as.factor(dataNo$JobSatisfaction)
dataNo$EnvironmentSatisfaction <- as.factor(dataNo$EnvironmentSatisfaction)
dataNo$WorkLifeBalance <- as.factor(dataNo$WorkLifeBalance)

FemalesYWithDistanceAndEnvS <- ggplot(data = dataYes[dataYes$Gender == "Female" , ], aes(y = DistanceFromHome, x = JobSatisfaction,color= JobSatisfaction)) + geom_boxplot()
FemalesYWithDistanceAndEnvS

FemalesNWithDistanceAndEnvS <- ggplot(data = dataNo[dataNo$Gender == "Female" , ], aes(y = DistanceFromHome, x = EnvironmentSatisfaction,color= EnvironmentSatisfaction)) + geom_boxplot()
FemalesNWithDistanceAndEnvS

MalesWithDistanceAndEnvS <- ggplot(data = data[data$Gender == "Male" , ], aes(y = DistanceFromHome, x = EnvironmentSatisfaction, color = EnvironmentSatisfaction)) + geom_boxplot()
MalesWithDistanceAndEnvS

# elli alo yes el HR a2al average laken elli 2alo No mafeesh far2 awi been el data 
MonthlyIncomeVSDep <- ggplot(data = dataYes, aes(y= MonthlyIncome,x=Department, color = Department)) + geom_boxplot()
MonthlyIncomeVSDep+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")

MonthlyIncomeVSDep <- ggplot(data = dataNo, aes(y= MonthlyIncome,x=Department, color = Department)) + geom_boxplot()
MonthlyIncomeVSDep+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")

MonthlyIncomeVSRole <- ggplot(data = dataYes, aes(y= MonthlyIncome,x=JobRole, color = JobRole)) + geom_boxplot()
MonthlyIncomeVSRole+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")

MonthlyIncomeVSRole <- ggplot(data = dataNo, aes(y= MonthlyIncome,x=JobRole, color = JobRole)) + geom_boxplot()
MonthlyIncomeVSRole+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")


HRYDepVSAvgWorkingHrs <- ggplot(data = dataYes[dataYes$Department == "Human Resources",], aes(AverageWorkingHours)) + geom_density()
HRYDepVSAvgWorkingHrs

HRNDepVSAvgWorkingHrs <- ggplot(data = dataNo[dataNo$Department == "Human Resources",], aes(AverageWorkingHours)) + geom_density()
HRNDepVSAvgWorkingHrs

## elli 2alo no mo3zamhom bio3odo a2al men 8 hours.
## elli 2alo yes fi kteer menhom bio3od aktar men 8 hours.

######################## END of Experimental Insights ########################


######################## Correlation ########################

# Get All non cat data
nonCatArr <- c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike","rateOfAttrition","YearsSinceLastPromotion","YearsAtCompany","TrainingTimesLastYear","AverageWorkingHours")
TempData <- data
TempData$Age = as.numeric(NonCatData$Age)
TempData$DistanceFromHome = as.numeric(NonCatData$DistanceFromHome)
TempData$MonthlyIncome = as.numeric(NonCatData$MonthlyIncome)
TempData$PercentSalaryHike = as.numeric(NonCatData$PercentSalaryHike)
TempData$YearsSinceLastPromotion = as.numeric(NonCatData$YearsSinceLastPromotion)
TempData$YearsAtCompany = as.numeric(NonCatData$YearsAtCompany)
TempData$TrainingTimesLastYear = as.numeric(NonCatData$TrainingTimesLastYear)


# Correlation matrix
data$Attrition <- as.numeric(ifelse(data$Attrition == "Yes" , 1, 0))
corMat <- cor(cbind(TempData[nonCatArr], TempData$Attrition))

