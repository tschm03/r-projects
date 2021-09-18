#load dataset into RStudio
banks.df <- read.csv("banks.csv")

#remove ID column
banks.df <- banks.df[,-1]

## Explorative Functions

#display column names
t(t(names(banks.df)))

#check column for data type
typeof(banks.df$Financial.Condition)

#view sample of data
head(banks.df)

#summary of columns
summary(banks.df)

#check for NAs
sum(is.na(banks.df))

## Logistic regression model

#create regression model
logit.reg <- glm(Financial.Condition ~ .
                 ,data = banks.df
                 ,family = "binomial")
options(scipen=999)

#view model summary
summary(logit.reg)

## Plotting the regression model

#load libraries
library(caret)
library(gains)
library(e1071)

#create prediction
logit.reg.pred <- predict(logit.reg, data = banks.df, type = "response")

#create confusion matrix & set cutoff value to 0.5
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)),
                as.factor(banks.df$Financial.Condition))

#create gains chart for lift chart
gain <- gains(banks.df$Financial.Condition, logit.reg.pred, groups = 100)

#plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(banks.df$Financial.Condition))
     ~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(banks.df$Financial.Condition))~c(0,dim(banks.df)[1]),lty=2)
