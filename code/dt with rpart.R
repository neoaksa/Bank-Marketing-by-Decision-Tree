setwd("~/Google Drive/gvsu/course/CIS678/Bank Marketing by Decision Tree/data/bank")
# Read CSV into R
MyData <- read.csv(file="bank-full.csv", header=TRUE, sep=";")
table(MyData$y)
# delete month
MyData = subset(MyData,select = -c(month,contact,duration))
# shuffle data
MyData <- MyData[sample(1:nrow(MyData)),]
# test data
MyValidation <- MyData[40001:45211,]
MyTrain <- MyData[1:40000,]
library(ROSE)
# MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "over",N = 60000)$data
MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "under",N = 20000, seed =1)$data
# MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "both",N = 30000,  p=0.5,seed=1)$data

# shuffle data
MyTrain <- MyTrain[sample(1:nrow(MyTrain)),]
library(C50)
x <- MyTrain[,1:13]
y <- MyTrain[,14]
table(y)

MyValidation <- ovun.sample(y ~ ., data = MyValidation, method = "both",N = 10000, p=0.5,seed=1)$data
# shuffle data
MyValidation <- MyData[sample(1:nrow(MyValidation)),]

testx <- MyValidation[,1:13]
testy <- MyValidation[,14]

table(testy)

# grow tree
fit <- C5.0(x,y, control = C5.0Control(earlyStopping = TRUE, minCases = 10))

# printcp(fit) # display the results
# plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# validation
p <- predict(fit, testx, type="class")
sum(p==testy)/length(p)
plot(p,testy)
plot(fit)
# p <- predict(fit, testx, type="class")
# p=as.vector(p)
# p = replace(p,p == 'no',0)
# p = replace(p,p == 'yes',1)
# testy.list = as.vector(testy)
# testy.list = replace(testy.list,testy.list == 'no',0)
# testy.list = replace(testy.list,testy.list == 'yes',1)
# preds <- prediction(as.list(p), as.list(testy))

# library(ROCR)
# # Recall-Precision curve     
# RP.perf <- performance(p,"prec", "rec")
# plot(RP.perf )
# # ROC curve
# ROC.perf <- performance(p, "tpr", "fpr");
# plot (ROC.perf);
# 
# # ROC area under the curve
# auc.tmp <- performance(p,"auc");
# auc <- as.numeric(auc.tmp@y.values)
# plot(fit)

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(fit, file = "c:/tree.ps",
     title = "Classification Tree for Kyphosis")        



