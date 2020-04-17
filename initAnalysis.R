# clean environment 
rm(list=ls())
# imports
library(ggplot2)

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
TotalWorkingYears <- TotalWorkingYears + geom_density(fill = "steelblue", color ="steelblue") 
TotalWorkingYears
# YearsAtCompany
YearsAtCompany <- ggplot(data = data, aes(YearsAtCompany))
YearsAtCompany <- YearsAtCompany + geom_density(fill = "steelblue", color ="steelblue") 
YearsAtCompany
# YearsSinceLastPromotion
YearsSinceLastPromotion <- ggplot(data = data, aes(YearsSinceLastPromotion))
YearsSinceLastPromotion <- YearsSinceLastPromotion + geom_density(fill = "steelblue", color ="steelblue") 
YearsSinceLastPromotion
# YearsWithCurrManager
YearsWithCurrManager <- ggplot(data = data, aes(YearsWithCurrManager))
YearsWithCurrManager <- YearsWithCurrManager + geom_density(fill = "steelblue", color ="steelblue") 
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


###########################
# Age vs Gender
AgeVGender <- ggplot(data = data, aes(x = Gender, y = Age))
AgeVGender <- AgeVGender + geom_boxplot(aes(fill = Gender))
AgeVGender
# Attrition Vs Age
AttVAge <- ggplot(data=data, aes(x = Age))
AttVAge <- AttVAge + geom_density(aes(color = Attrition)) 
AttVAge
# Attrition Vs Gender
AttVGender <- ggplot(data = data, aes(Attrition))
AttVGender <- AttVGender + geom_bar(aes(fill=Gender)) 
AttVGender
# Attrition Vs Age & Gender
AttVAgeAndGender <- ggplot(data = data, aes(x = Attrition, y = Age))
AttVAgeAndGender <- AttVAgeAndGender + geom_boxplot(aes(fill = Gender))
AttVAgeAndGender
# BusinessTravel Vs Age & Gender
TravelVAgeAndGender <- ggplot(data = data, aes(x = BusinessTravel, y = Age))
TravelVAgeAndGender <- TravelVAgeAndGender + geom_boxplot(aes(fill = Gender))
TravelVAgeAndGender
# Attrition Vs BusinessTravel
TravVAttr <- ggplot(data = data, aes(Attrition))
TravVAttr <- TravVAttr + geom_bar(aes(fill = BusinessTravel)) 
TravVAttr
# DistanceFromHome and gender
DistVGender <- ggplot(data = data, aes(DistanceFromHome))
DistVGender <- DistVGender + geom_density(aes(color = Gender))
DistVGender
# Attrition Vs DistanceFromHome
AttrVDist <- ggplot(data = data, aes(DistanceFromHome))
AttrVDist <- AttrVDist + geom_density(aes(color = Attrition))
AttrVDist
# Attrition Vs DistanceFromHome, Gender
AttVDistGender <- ggplot(data = data, aes(x = Attrition, y = DistanceFromHome))
AttVDistGender <- AttVDistGender + geom_boxplot(aes(fill = Gender))
AttVDistGender
# Attrition Vs Education
AttrVEdu <- ggplot(data = data, aes(Education, fill=Attrition))
AttrVEdu <- AttrVEdu + geom_bar(position=position_dodge())
AttrVEdu





