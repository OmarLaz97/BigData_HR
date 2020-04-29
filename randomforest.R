#install.packages("randomForest")
library(randomForest)
library(ROCR)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)
rm(list=ls())


data <- read.csv("employee_data.csv")
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

#TempData <- dummy_cols(TempData, select_columns = CatArray, remove_first_dummy = TRUE, remove_selected_columns = TRUE)


# we have 4300 rows: 3010 (70%) training and the rest for testing 
# Logistic Regression

trainingSet <- head(TempData, 3010)
testSet <- tail(TempData, nrow(TempData)-3010)

model1 <- randomForest(Attrition ~ ., data = trainingSet, importance = TRUE)
model1

model2 <- randomForest(Attrition ~ ., data = trainingSet, ntree = 500, mtry = 6, importance = TRUE)
model2





newdata1 <- testSet[ , !(names(TempData) %in% c("Attrition"))]
newdata1$Attrition <- predict (model2,newdata=newdata1,type="response")

newdata1$Attrition <- factor(ifelse(newdata1$Attrition >= 0.5, 1, 0))

confusionMatrix <- table( newdata1$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy
