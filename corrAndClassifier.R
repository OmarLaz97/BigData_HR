# clean environment 
rm(list=ls())

data <- read.csv("employee_data.csv")

######################## Correlation ########################

# Get All non cat data
nonCatArr <- c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike","rateOfAttrition","YearsSinceLastPromotion","YearsAtCompany","TrainingTimesLastYear","AverageWorkingHours")
TempData <- data
TempData$Age = as.numeric(TempData$Age)
TempData$DistanceFromHome = as.numeric(TempData$DistanceFromHome)
TempData$MonthlyIncome = as.numeric(TempData$MonthlyIncome)
TempData$PercentSalaryHike = as.numeric(TempData$PercentSalaryHike)
TempData$YearsSinceLastPromotion = as.numeric(TempData$YearsSinceLastPromotion)
TempData$YearsAtCompany = as.numeric(TempData$YearsAtCompany)
TempData$TrainingTimesLastYear = as.numeric(TempData$TrainingTimesLastYear)

# Correlation matrix
TempData$Attrition <- as.numeric(ifelse(TempData$Attrition == "Yes" , 1, 0))
corr <- cor(cbind(TempData[nonCatArr], TempData$Attrition))
corMat <- corrplot.mixed(corr, upper = "ellipse", tl.cex = 0.40, tl.pos = 'd')
corMat
