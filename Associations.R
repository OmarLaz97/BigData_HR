rm(list=ls())
library(arules)
library(fastDummies)
#library(arulesViz)

data <- read.csv("employee_data.csv")

# Categorical data to transform to n-1 variables
CatArray <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus")


#categorize monthly income into 3 categories
income<-summary(data$MonthlyIncome)
data$MonthlyIncome<-cut(data$MonthlyIncome, c(10090,29260,65060,199990), labels = c("low","medium","high"))

#average working hours (split to above average and below average)
workingHours<-summary(data$AverageWorkingHours)
data$AverageWorkingHours<-cut(data$AverageWorkingHours, c(workingHours["Min."],workingHours["Mean"],workingHours["Max."]), labels = c("below average","above average"))

#age
Age<-summary(data$Age)
data$Age<-cut(data$Age, c(Age["Min."],Age["1st Qu."],Age["3rd Qu."],Age["Max."]))

#distance from home 
Distance<-summary(data$DistanceFromHome)
data$DistanceFromHome<-discretize(data$DistanceFromHome,breaks=3)

#number of companies worked for
Companies<-summary(data$NumCompaniesWorked)
data$NumCompaniesWorked<-discretize(data$NumCompaniesWorked,breaks=3)

#years at company
yearsCompany<-summary(data$YearsAtCompany)
data$YearsAtCompany<-discretize(data$YearsAtCompany,breaks=3)

#years with current manager
yearsManager<-summary(data$YearsWithCurrManager)
data$YearsWithCurrManager<-discretize(data$YearsWithCurrManager,breaks=3)

# inspecting rules for work life balance <3

assocData<-data[data[, "WorkLifeBalance"] < 3,]
assocData<-assocData[,c("DistanceFromHome","MaritalStatus","AverageWorkingHours","WorkLifeBalance","MonthlyIncome")]


#attrition with the most correlated variables

#assocData<-data[,c("MaritalStatus","AverageWorkingHours","Age","TotalWorkingYears","Attrition")]

#Job satisfaction >=3
if (FALSE){
  assocData<-data[data[,"JobSatisfaction"]>=3,]
  assocData<-assocData[,c("JobRole","Department","JobSatisfaction","YearsWithCurrManager","BusinessTravel","MonthlyIncome")]
  
}

names <- c(1:5)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
rules_subset <- subset(rules, (rhs %in% paste0("WorkLifeBalance=", unique(assocData$WorkLifeBalance))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
#plot(rules_subset, measure=c("support","confidence"),shading="lift")