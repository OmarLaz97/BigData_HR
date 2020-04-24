rm(list=ls())
library(arules)
data <- read.csv("employee_data.csv")
rules<-apriori(data, parameter = list(supp = 0.01,minlen=2, conf = 0.5, target = "rules"))
