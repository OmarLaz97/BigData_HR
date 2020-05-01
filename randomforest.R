#install.packages("randomForest")
library(randomForest)
library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)
rm(list=ls())


TempData <- read.csv("employee_data.csv")

# Shuffle Data
set.seed(42)
rows <- sample(nrow(TempData))
TempData <- TempData[rows, ]

# Remove first column (employee ID)
TempData <- TempData[-c(1)]

#Set Attrition Yes or No to 1 or 0
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))

#Set Busniess Travel to 0: No travel, 1: rarely, 2: frequently
TempData$BusinessTravel <- ifelse(TempData$BusinessTravel == "Non-Travel", 0, ifelse(TempData$BusinessTravel == "Travel_Rarely", 1, 2))

# we have 4300 rows: 3010 (70%) training and the rest for testing 
trainingSet <- head(TempData, 3010)
testSet <- tail(TempData, nrow(TempData)-3010)


# Model 1 with default mtry = 8
model1 <- randomForest(Attrition ~ ., data = trainingSet, importance = TRUE)
model1

# Model 2 with mtry = 6
model2 <- randomForest(Attrition ~ ., data = trainingSet, ntree = 500, mtry = 6, importance = TRUE)
model2


# Get a copy of test set without attrition column
newdata1 <- testSet[ , !(names(TempData) %in% c("Attrition"))]

# Predict attrition using Model 2
newdata1$Attrition <- predict (model2,newdata=newdata1,type="response")

# Threshold = 0.5 to set attrition to 0 or 1
newdata1$Attrition <- factor(ifelse(newdata1$Attrition >= 0.5, 1, 0))

confusionMatrix <- table( newdata1$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy
