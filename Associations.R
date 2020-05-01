rm(list=ls())
library(arules)
library(fastDummies)
library(arulesViz)

data <- read.csv("employee_data.csv")

#converting continuous variables into categorical variables to create transactions

#monthly income 
income<-summary(data$MonthlyIncome)
data$MonthlyIncome<-cut(data$MonthlyIncome, c(10090,29260,65060,199990), labels = c("low","medium","high"))

#average working hours 
workingHours<-summary(data$AverageWorkingHours)
data$AverageWorkingHours<-cut(data$AverageWorkingHours, c(workingHours["Min."],workingHours["Mean"],workingHours["Max."]), labels = c("below average","above average"))

#age
Age<-summary(data$Age)
data$Age<-cut(data$Age, c(Age["Min."],Age["1st Qu."],Age["3rd Qu."],Age["Max."]))

#distance from home 
data$DistanceFromHome<-discretize(data$DistanceFromHome,breaks=3)

#number of companies worked for
data$NumCompaniesWorked<-discretize(data$NumCompaniesWorked,breaks=3)

#years at company
data$YearsAtCompany<-discretize(data$YearsAtCompany,breaks=3)

#years with current manager
data$YearsWithCurrManager<-discretize(data$YearsWithCurrManager,breaks=3)

#1) Rules using all columns except EmployeeID
assocData<-data[,c(2:27)]
#factorize all columns
names <- c(1:26)
assocData[,names] <- lapply(assocData[,names] , factor)
#convert the data into transactions
transactions<-as(assocData,"transactions")
#get association rules using apriori algorithm
rules<-apriori(transactions, parameter = list(supp = 0.05,minlen=3, conf = 0.05, target = "rules",maxtime=0))
#inspect top 6 rules sorted by lift
lift<-head(sort(rules,by="lift"),6)
inspect(lift)
#plot the rules' support, confidence and lift distributions
plot(rules, measure=c("support","confidence"),shading="lift")

#2) inspecting rules for work life balance <3
#First trial 
assocData<-data[data[, "WorkLifeBalance"] < 3,]
assocData<-assocData[,c("DistanceFromHome","MaritalStatus","AverageWorkingHours","WorkLifeBalance","MonthlyIncome")]
names <- c(1:5)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
#only consider rules with work life balance in the RHS
rules_subset <- subset(rules, (rhs %in% paste0("WorkLifeBalance=", unique(assocData$WorkLifeBalance))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
plot(rules_subset, measure=c("support","confidence"),shading="lift")

#Second trial

assocData<-data[data[, "WorkLifeBalance"] < 3,]
assocData<-assocData[,c("DistanceFromHome","MaritalStatus","AverageWorkingHours","WorkLifeBalance","MonthlyIncome","JobRole")]
names <- c(1:6)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
#only consider rules with work life balance in the RHS
rules_subset <- subset(rules, (rhs %in% paste0("WorkLifeBalance=", unique(assocData$WorkLifeBalance))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
plot(rules_subset, measure=c("support","confidence"),shading="lift")

#3) Job satisfaction >=3

assocData<-data[data[,"JobSatisfaction"]>=3,]
assocData<-assocData[,c("JobRole","Department","JobSatisfaction","YearsWithCurrManager","BusinessTravel","MonthlyIncome")]
names <- c(1:6)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
rules_subset <- subset(rules, (rhs %in% paste0("JobSatisfaction=", unique(assocData$JobSatisfaction))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
plot(rules_subset, measure=c("support","confidence"),shading="lift")

#4)Attrition with the most correlated variables

assocData<-data[,c("MaritalStatus","AverageWorkingHours","Age","TotalWorkingYears","Attrition","NumCompaniesWorked","BusinessTravel")]
names <- c(1:7)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
rules_subset <- subset(rules, (rhs %in% paste0("Attrition=", unique(assocData$Attrition))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
plot(rules_subset, measure=c("support","confidence"),shading="lift")

#5)Attriton with the variables we were expecting to be of most significance 
assocData<-data[,c("MonthlyIncome","DistanceFromHome","AverageWorkingHours","JobSatisfaction","WorkLifeBalance","Attrition")]
names <- c(1:6)
assocData[,names] <- lapply(assocData[,names] , factor)
transactions<-as(assocData,"transactions")
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=3, conf = 0.01, target = "rules",maxtime=0))
rules_subset <- subset(rules, (rhs %in% paste0("Attrition=", unique(assocData$Attrition))))
lift<-head(sort(rules_subset,by="lift"),6)
inspect(lift)
plot(rules_subset, measure=c("support","confidence"),shading="lift")