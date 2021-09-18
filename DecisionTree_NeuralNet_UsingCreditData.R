##Import and Exploring Data

#import csv
credit.df <- read.csv("GermanCredit.csv")

#display column names
t(t(colnames(credit.df)))

#examine structure of the data
str(credit.df)

#remove unwanted columns
credit.df <- credit.df[,c(2,3,9,11,12,13,21,23,26,29,32)]

#check for nulls
lapply(credit.df, function(x) {length(which(is.na(x)))})

#data summary
summary(credit.df)

#convert CHK_ACCT, SAV_ACCT, EMPLOYMENT to factors
credit.df$CHK_ACCT <- as.factor(credit.df$CHK_ACCT)
credit.df$SAV_ACCT <- as.factor(credit.df$SAV_ACCT)
credit.df$EMPLOYMENT <- as.factor(credit.df$EMPLOYMENT)

#review histogram on AMOUNT for skew in data
hist(credit.df$AMOUNT, xlab="Amount", main="", col="dodgerblue2")


## Partition data into training & validation sets
set.seed(1)
train.index <- sample(dim(credit.df)[1],
                      dim(credit.df)[1] * 0.6)
train.df <- credit.df[train.index,]
valid.df <- credit.df[-train.index,]


## Creating Classification Tree

#load necessary packages
library(rpart)
library(rpart.plot)

#create classification tree using rpart()
credit.tree <- rpart(RESPONSE ~ ., data=train.df, method="class")

#plot tree using prp()
prp(credit.tree, type=1, extra=1, under=TRUE, split.font=2,
    varlen=-10, box.col=ifelse(credit.tree$frame$var=="<leaf>",'gray','white'))

## Making predictions using classification tree

#load necessary library
library(caret)

#make predictions based on training data
credit.tree.train.pred <- predict(credit.tree, train.df, type="class")

#display confusion matrix for training predictions
confusionMatrix(credit.tree.train.pred, factor(train.df$RESPONSE))

#make predictions based on validation data
credit.tree.valid.pred <- predict(credit.tree, valid.df, type="class")

#display confusion matrix for training predictions
confusionMatrix(credit.tree.valid.pred, factor(valid.df$RESPONSE))


## Creating and Plotting Neural Network

#load necessary library
library(neuralnet)

#convert qualitative fields to be used to factor
credit.df$REAL_ESTATE <- factor(credit.df$REAL_ESTATE)
credit.df$OWN_RES <- factor(credit.df$OWN_RES)


#create training & validation sets
train.df <- credit.df[train.index,]
valid.df <- credit.df[-train.index,]

#transform qualitative fields into dummy variables
nn.train <- as.data.frame(model.matrix( ~ RESPONSE + CHK_ACCT +
                          EMPLOYMENT + REAL_ESTATE, data=train.df))
nn.valid <- as.data.frame(model.matrix( ~ RESPONSE + CHK_ACCT +
                          EMPLOYMENT + REAL_ESTATE, data=valid.df))

#run neural network using neuralnet() and training data
#using 2 hidden layers
credit.nn <- neuralnet(RESPONSE ~ CHK_ACCT1 + CHK_ACCT2 + CHK_ACCT3 + 
                       EMPLOYMENT1 + EMPLOYMENT2 + 
                       EMPLOYMENT3 + EMPLOYMENT4 + REAL_ESTATE1,
                       data=nn.train, hidden=2)

#plot neural network
plot(credit.nn, rep="best")


## Predict and Validate Neural Network

#create training predictions
train.pred.nn=compute(credit.nn, nn.train)
train.class <- ifelse(train.pred.nn$net.result>0.5,1,0)

#analyze training predictions with confusion matrix
confusionMatrix(factor(train.class),
                factor(nn.train$RESPONSE))

#create validation predictions
valid.pred.nn=compute(credit.nn, nn.valid)
valid.class <- ifelse(valid.pred.nn$net.result>0.5,1,0)

#analyze validation predictions with confusion matrix
confusionMatrix(factor(valid.class),
                factor(nn.valid$RESPONSE))

#create ROC curve for validation predictions
library(pROC)
valid.class <- as.vector(valid.class)
r <- roc(nn.valid$RESPONSE, valid.class)

#plot ROC curve
plot.roc(r)

#produce AUC
auc(r)
