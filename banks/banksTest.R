setwd("~/Documents/analytics/banks")

banks <- read.csv2("bank.csv", stringsAsFactors = TRUE)
summary(banks)

library(dplyr)
library(ggplot2)
library(tidyr)

banks %>% group_by(education) %>% summarise(yes=sum(y=="yes"), total=n()) %>% gather(value, count, -education) %>% ggplot(aes(x=education, y=count, fill=value)) + geom_bar(stat = "identity", position = "dodge")

banks %>% group_by(education) %>% summarise(yes=sum(y=="yes")/n(), no=sum(y=="no")/n()) %>% gather(value, count, -education) %>% ggplot(aes(x=education, y=count, fill=value)) + geom_bar(stat = "identity") + theme_bw()


# customers not contacted in previous campaigns
banks %>% 
banks %>% filter(pdays==-1) %>% group_by(previous, y) %>% summarise(n=n())
banks %>% filter(pdays==-1) %>% group_by(poutcome, y) %>% summarise(n=n())

banks %>% mutate(contacted=ifelse(previous==0, 1, 0)) %>% group_by(contacted, previous) %>% summarise(n=n())

# https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions



plotCategory(banks, "education")

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)


