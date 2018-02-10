# Read CSV into R
MyData <- read.csv(file="bank-full.csv", header=TRUE, sep=";")
class(MyData)
# delete month
MyData = subset(MyData,select = -c(month,contact))
# shuffle data
MyData <- MyData[sample(1:nrow(MyData)),]
library(C50)
x <- MyData[1:40000,1:14]
y <- MyData[1:40000,15]

testx <- MyData[40001:45211,1:14]
testy <- MyData[40001:45211,15]

# grow tree
fit <- C5.0(x,y, control = C5.0Control(earlyStopping = TRUE, minCases = 50))

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

p <- predict(fit, testx, type="class")
sum(p==testy)/length(p)
plot(fit)

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(fit, file = "c:/tree.ps",
     title = "Classification Tree for Kyphosis")          