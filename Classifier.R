library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)

# clean environment 
rm(list=ls())


data <- read.csv("employee_data.csv")

# Categorical data to transform to n-1 variables
CatArray <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus")

TempData <- data

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(TempData))

# Randomly order data
TempData <- TempData[rows, ]

TempData <- TempData[-c(1)]

#Set Attrition Yes or No to 1 or 0
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))


#Set Busniess Travel to 0: No travel, 1: rarely, 2: frequently
TempData$BusinessTravel <- ifelse(TempData$BusinessTravel == "Non-Travel", 0, ifelse(TempData$BusinessTravel == "Travel_Rarely", 1, 2))

TempData <- dummy_cols(TempData, select_columns = CatArray, remove_first_dummy = TRUE, remove_selected_columns = TRUE)


# we have 4300 rows: 3010 (70%) training and the rest for testing 
# Logistic Regression

trainingSet <- head(TempData, 3010)
testSet <- tail(TempData, nrow(TempData)-3010)


basicModel <- glm(Attrition ~.,
               data =trainingSet, family=binomial(link="logit"),
               na.action=na.pass)
summary(basicModel) 

model1 <- stepAIC(basicModel, direction = "both")
summary(model1)
# AIC: 2118.6
# Removing multicollinearity through VIF check
vif(model1)


model2 <- glm(formula = Attrition ~ Age + BusinessTravel + Education + 
                 JobLevel + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + JobInvolvement + AverageWorkingHours + 
                 `Department_Research & Development` + Department_Sales + 
                 `EducationField_Technical Degree` + JobRole_Manager + `JobRole_Manufacturing Director` + 
                 `JobRole_Research Director` + MaritalStatus_Married + MaritalStatus_Single, 
               family = binomial(link = "logit"), data = trainingSet, na.action = na.pass)
summary(model2)
#Accuracy = 85.73643


# Remove EducationField_Technical Degree`   -0.311127   0.221286  -1.406 0.159726  
model2 <- glm(formula = Attrition ~ Age + BusinessTravel + Education + 
                JobLevel + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + JobInvolvement + AverageWorkingHours + 
                `Department_Research & Development` + Department_Sales 
                 + JobRole_Manager + `JobRole_Manufacturing Director` + 
                `JobRole_Research Director` + MaritalStatus_Married + MaritalStatus_Single, 
              family = binomial(link = "logit"), data = trainingSet, na.action = na.pass)
summary(model2)
#Accuracy = 85.81395


# Remove JobLevel                            -0.081963   0.051989  -1.577 0.114900  
model2 <- glm(formula = Attrition ~ Age + BusinessTravel + Education + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + JobInvolvement + AverageWorkingHours + 
                `Department_Research & Development` + Department_Sales 
              + JobRole_Manager + `JobRole_Manufacturing Director` + 
                `JobRole_Research Director` + MaritalStatus_Married + MaritalStatus_Single, 
              family = binomial(link = "logit"), data = trainingSet, na.action = na.pass)
summary(model2)
#Accuracy = 85.81395


# Remove JobInvolvement                      -0.128233   0.078656  -1.630 0.103041  
model2 <- glm(formula = Attrition ~ Age + BusinessTravel + Education + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + AverageWorkingHours + 
                `Department_Research & Development` + Department_Sales 
              + JobRole_Manager + `JobRole_Manufacturing Director` + 
                `JobRole_Research Director` + MaritalStatus_Married + MaritalStatus_Single, 
              family = binomial(link = "logit"), data = trainingSet, na.action = na.pass)
summary(model2)
#Accuracy = 85.73643


pred = predict(model2, type="response") # this returns the probability scores on the training data
predObj = prediction(pred, trainingSet$Attrition) # prediction object needed by ROCR

rocObj = performance(predObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object

auc = aucObj@y.values[[1]]
auc   # the auc score: tells you how well the model predicts.

plot(rocObj, main = paste("Area under the curve:", auc))

newdata1 <- testSet[ , !(names(TempData) %in% c("Attrition"))]
newdata1$Attrition <- predict (model2,newdata=newdata1,type="response")

newdata1$Attrition <- factor(ifelse(newdata1$Attrition >= 0.5, 1, 0))

confusionMatrix <- table( newdata1$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy


