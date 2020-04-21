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
testSet <- tail(dfm, nrow(data)-3010)

mylogit <- glm(Attrition ~.,
               data =trainingSet, family=binomial(link="logit"),
               na.action=na.pass)

