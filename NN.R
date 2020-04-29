install.packages('neuralnet')
library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)
library(neuralnet)
rm(list=ls())


data <- read.csv("employee_data.csv")

# Categorical data to transform to n-1 variables
CatArray <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus")

TempData <- data
rows <- sample(nrow(TempData))

# Randomly order data
TempData <- TempData[rows, ]

TempData <- TempData[-c(1)]

#Set Attrition Yes or No to 1 or 0
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))


#Set Busniess Travel to 0: No travel, 1: rarely, 2: frequently
TempData$BusinessTravel <- ifelse(TempData$BusinessTravel == "Non-Travel", 0, ifelse(TempData$BusinessTravel == "Travel_Rarely", 1, 2))
#EducFieldScience+EducFieldMarketing+EducFieldMed+EducFieldOther+EducFieldTech+EducFieldTech+JobRoleExec+JobRoleResScienr+JobRoleHR+JobRoleLabTech+JobRoleManager+JobRoleRdir+JobRoleManDir
TempData <- dummy_cols(TempData, select_columns = CatArray, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
names(TempData)[names(TempData)=="Department_Research & Development"]<-"DepRes"
names(TempData)[names(TempData)=="EducationField_Life Sciences"]<-"EducFieldScience"
names(TempData)[names(TempData)=="EducationField_Marketing"]<-"EducFieldMarketing"
names(TempData)[names(TempData)=="Gender_Male"]<-"GendMale"
names(TempData)[names(TempData)=="JobRole_Research Director"]<-"JobRoleRdir"
names(TempData)[names(TempData)=="JobRole_Manufacturing Director"]<-"JobRoleManDir"
names(TempData)[names(TempData)=="EducationField_Medical"]<-"EducFieldMed"
names(TempData)[names(TempData)=="EducationField_Other"]<-"EducFieldOther"
names(TempData)[names(TempData)=="JobRole_Human Resources"]<-"JobRoleHR"




# we have 4300 rows: 3010 (70%) training and the rest for testing 
# Logistic Regression
maxs <- apply(TempData, 2, max)
mins <- apply(TempData, 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(TempData,center = mins, scale = maxs - mins))

trainingSet <- head(scaled.data, 3010)
testSet <- tail(scaled.data, nrow(TempData)-3010)


set.seed(2)
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
NN = neuralnet( formula = Attrition ~ Age +BusinessTravel+ GendMale+DistanceFromHome + Education + 
                  JobLevel+ MonthlyIncome + NumCompaniesWorked + PercentSalaryHike +StockOptionLevel + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + JobInvolvement +PerformanceRating + AverageWorkingHours + Department_Sales+MaritalStatus_Married+JobRole_Manager + MaritalStatus_Single+ DepRes+EducFieldScience+EducFieldMarketing+JobRoleRdir+JobRoleManDir+EducFieldMed+EducFieldOther+JobRoleHR, trainingSet, hidden = 2 ,linear.output = T, act.fct = sigmoid, stepmax = 1e+06)

NN2 = neuralnet( formula = Attrition ~ Age +BusinessTravel+ GendMale+DistanceFromHome + Education + 
                   JobLevel+ MonthlyIncome + NumCompaniesWorked + PercentSalaryHike +StockOptionLevel + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + JobInvolvement +PerformanceRating + AverageWorkingHours + Department_Sales+MaritalStatus_Married+JobRole_Manager + MaritalStatus_Single+ DepRes+EducFieldScience+EducFieldMarketing+JobRoleRdir+JobRoleManDir+EducFieldMed+EducFieldOther+JobRoleHR, trainingSet, hidden = 3,stepmax = 1e+06 ,linear.output = T )

newdata1 <- testSet[ , !(names(TempData) %in% c("Attrition"))]
newdata1$Attrition <- predict (NN,newdata=newdata1,type="response")

newdata1$Attrition <- factor(ifelse(newdata1$Attrition >= 0.5, 1, 0))

confusionMatrix <- table( newdata1$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy


newdata2 <- testSet[ , !(names(TempData) %in% c("Attrition"))]
newdata2$Attrition <- predict (NN2,newdata=newdata2,type="response")

newdata2$Attrition <- factor(ifelse(newdata2$Attrition >= 0.5, 1, 0))

confusionMatrix <- table( newdata2$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy