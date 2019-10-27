setwd("~/Documents/analytics/banks")

banks <- read.csv2("bank.csv", stringsAsFactors = TRUE)
summary(banks)

library(dplyr)
library(ggplot2)
library(tidyr)

banks %>% group_by(education) %>% summarise(yes=sum(y=="yes"), total=n()) %>% gather(value, count, -education) %>% ggplot(aes(x=education, y=count, fill=value)) + geom_bar(stat = "identity", position = "dodge")

banks %>% group_by(education) %>% summarise(yes=sum(y=="yes")/n(), no=sum(y=="no")/n()) %>% gather(value, count, -education) %>% ggplot(aes(x=education, y=count, fill=value)) + geom_bar(stat = "identity") + theme_bw()


# customers not contacted in previous campaigns
banks %>% filter(pdays==-1) %>% group_by(previous, y) %>% summarise(n=n())
banks %>% filter(pdays==-1) %>% group_by(poutcome, y) %>% summarise(n=n())

banks %>% mutate(contacted=ifelse(previous==0, 1, 0)) %>% group_by(contacted, previous) %>% summarise(n=n())

# https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions


banks <- banks %>% mutate(id=1:nrow(banks))
banks <- banks %>% mutate(contacted = ifelse(pdays!=-1, "yes", "no"))

banks %>% group_by(month, contacted) %>% summarise(count=n()) %>% ggplot(aes(x=month, y=count, fill=contacted)) + geom_bar(stat = "identity", position = "dodge")

library(caret)

set.seed(1014)
a <- createDataPartition(banks$y, p=0.8, list = FALSE)
train <- banks[a,]
test <- banks[-a,]

partModel <- function(formula, traindata, testdata, method="rpart"){
  
  model <- train(formula, method="rpart", data=traindata)
  predictions <- predict(model, newdata=testdata)
  confusionMatrix(predictions, testdata$y)
  
}

partModel(y ~ age + job + marital + education, train, test)
partModel(y ~ default + balance + housing + loan, train, test)
partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test)
partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test)


partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test, "svmPoly")
partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test, "C5.0")
partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test, "nb")
partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train, test, "nnet")


banks.contacted <- banks %>% filter(contacted==1)

set.seed(1014)
b <- createDataPartition(banks.contacted$y, p=0.8, list = FALSE)
train.contacted <- banks.contacted[b,]
test.contacted <- banks.contacted[-b,]

partModel(y ~ contacted + pdays + previous + poutcome + contact + duration + campaign, train.contacted, test.contacted)
