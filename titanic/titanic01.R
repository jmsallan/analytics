setwd("~/Dropbox (UPC)/BA_docs/titanic")

#note:read with readr

test0 <- read.csv("test.csv", stringsAsFactors = TRUE)
survived <- read.csv("gender_submission.csv")
test <- merge(test0, survived)
train <- read.csv("train.csv", stringsAsFactors = FALSE)

#----working with train----

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <-as.factor(train$Embarked)

test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <-as.factor(test$Embarked)

summary(train)

#----looking for some initial tests----

with(train, table(Survived, Sex))
with(train, table(Survived, Pclass))
with(train, table(Survived, Embarked))

par(mfrow=c(2,2), main="Survived")
boxplot(Age ~ Survived, train, pch=16, ylab="age")
boxplot(SibSp ~ Survived, train, pch=16, ylab="SibSp")
boxplot(Parch ~ Survived, train, pch=16, ylab="Parch")
boxplot(Fare ~ Survived, train, pch=16, ylab="Fare")

d.AgeSurv <- density(train[which(train$Survived==1 &complete.cases(train)),]$Age)
d.AgeNotSurv <- density(train[which(train$Survived==0 &complete.cases(train)),]$Age)

par(mfrow=c(1,2))
plot(d.AgeSurv, main="Survived")
plot(d.AgeNotSurv, main="Not survived")


with(train, table(Sex, Pclass))

boxplot(Fare ~ Pclass, train, pch=16)

#----using caret----

library(caret)

test.complete <- test[complete.cases(test$Fare), ]

modFit <- train(Survived ~ Pclass + Sex + SibSp + Parch + Fare, method="rpart", data=train)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE)
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

predictions <- predict(modFit, newdata = test)
confusionMatrix(predictions, test.complete$Survived)
