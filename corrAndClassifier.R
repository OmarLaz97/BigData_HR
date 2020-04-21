library(ROCR)

# clean environment 
rm(list=ls())


data <- read.csv("employee_data.csv")

######################## Correlation ########################

# Get All non cat data
nonCatArr <- c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike", "NumCompaniesWorked",
"TotalWorkingYears", "YearsSinceLastPromotion","YearsAtCompany", "YearsWithCurrManager", 
"TrainingTimesLastYear","AverageWorkingHours")

TempData <- data

#Set Attrition Yes or No to 1 or 0
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))

#Convert All categorical Data to numbers
TempData<-data.matrix(TempData)
TempData <- as.data.frame(TempData)

#factor(TempData$BusinessTravel)

# Correlation matrix
corMat <- cor(cbind(TempData[nonCatArr], TempData$Attrition))
corrplot.mixed(corMat, upper = "ellipse", tl.cex = 0.40, tl.pos = 'd')


#################### Classifier ##################################
# we have 4300 rows: 3010 (70%) training and the rest for testing 
# Logistic Regression
corMat <- cor(TempData[ , !(names(TempData) %in% c("Attrition"))])
summary(corMat)

trainingSet <- head(data, 3010)
testSet <- tail(data, nrow(data)-3010)

mylogit <- glm(Attrition ~.,
               data =trainingSet, family=binomial(link="logit"),
               na.action=na.pass)
summary(mylogit) 


pred = predict(mylogit, type="response") # this returns the probability scores on the training data
predObj = prediction(pred, trainingSet$Attrition) # prediction object needed by ROCR

rocObj = performance(predObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object

auc = aucObj@y.values[[1]]
auc   # the auc score: tells you how well the model predicts.

plot(rocObj, main = paste("Area under the curve:", auc))

newdata1 <- testSet[ , !(names(TempData) %in% c("Attrition"))]
newdata1$Attrition <- predict (mylogit,newdata=newdata1,type="response")

newdata1$Attrition <- factor(ifelse(newdata1$Attrition >= 0.50, 1, 0))

confusionMatrix <- table( newdata1$Attrition, testSet$Attrition)
confusionMatrix

accuracy <- 100*(sum(diag(confusionMatrix))/sum(confusionMatrix))
accuracy


