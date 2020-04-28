rm(list=ls())
library(arules)
library(fastDummies)

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
data$DistanceFromHome<-discretize(data$DistanceFromHome,method = "cluster")

#number of companies worked for
Companies<-summary(data$NumCompaniesWorked)
data$NumCompaniesWorked<-discretize(data$NumCompaniesWorked,method = "cluster")

#years at company
yearsCompany<-summary(data$YearsAtCompany)
data$YearsAtCompany<-discretize(data$YearsAtCompany,method = "cluster")

#years with current manager
yearsManager<-summary(data$YearsWithCurrManager)
data$YearsWithCurrManager<-discretize(data$YearsWithCurrManager,method = "cluster")


assocData<-data
assocData[,c("EmployeeID","YearsWithCurrManager","PercentSalaryHike","StockOptionLevel","Department")]<-list(NULL)


names <- c(1:22)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
#inspect(transactions)
rules<-apriori(transactions, parameter = list(supp = 0.05,minlen=3, conf = 0.5, target = "rules"))
#inspect(rules)
support<-head(sort(rules, by="support"),6)
#inspect(support)
lift<-head(sort(rules,by="lift"),6)
inspect(lift)
