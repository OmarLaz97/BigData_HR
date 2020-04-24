rm(list=ls())
library(arules)
library(fastDummies)

data <- read.csv("employee_data.csv")

# Categorical data to transform to n-1 variables
CatArray <- c("Department", "EducationField", "Gender", "JobRole", "MaritalStatus")


#categorize monthly income into 4 categories
income<-summary(data$MonthlyIncome)
data$MonthlyIncome<-cut(data$MonthlyIncome, c(10090,29260,65060,199990), labels = c("low","medium","high"))

test<-data[c("MonthlyIncome","Gender","BusinessTravel","MaritalStatus")]
names <- c(1:4)
test[,names] <- lapply(test[,names] , factor)
transactions<-as(test,"transactions")
#inspect(transactions)
rules<-apriori(transactions, parameter = list(supp = 0.01,minlen=4, conf = 0.5, target = "rules"))
#inspect(rules)
support<-head(sort(rules, by="support"),6)
inspect(support)
