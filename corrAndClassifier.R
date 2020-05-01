library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)

# clean environment 
rm(list=ls())


TempData <- read.csv("employee_data.csv")

######################## Correlation ########################

# Get All non cat data
nonCatArr <- c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike", "NumCompaniesWorked",
"TotalWorkingYears", "YearsSinceLastPromotion","YearsAtCompany", "YearsWithCurrManager", 
"TrainingTimesLastYear","AverageWorkingHours")

# Categorical data to transform to n-1 variables
CatArray <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus")


TempData <- TempData[-c(1)]

#Set Attrition Yes or No to 1 or 0
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))


#Set Busniess Travel to 0: No travel, 1: rarely, 2: frequently
TempData$BusinessTravel <- ifelse(TempData$BusinessTravel == "Non-Travel", 0, ifelse(TempData$BusinessTravel == "Travel_Rarely", 1, 2))

TempData <- dummy_cols(TempData, select_columns = CatArray, remove_first_dummy = TRUE, remove_selected_columns = TRUE)


# Correlation matrix
corMat <- cor(cbind(TempData[nonCatArr], TempData$Attrition))
corrplot.mixed(corMat, upper = "ellipse", tl.cex = 0.40, tl.pos = 'd')


corMat <- cor(TempData)
corrplot.mixed(corMat, upper = "ellipse", tl.cex = 0.40, tl.pos = 'd')
summary(corMat)


