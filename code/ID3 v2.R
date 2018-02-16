library(data.tree)
setwd("~/Google Drive/gvsu/course/CIS678/Bank Marketing by Decision Tree/data/bank")
# check the pure, the class should be last column
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

# check the entropoy``
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

# cal gain
InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

# retrun p of majority  in a given vector
predict.outcome <- function(outcome.vector){
  # Predict class based on outcome.vector
  
  # Determine possible outcomes and percentage occurrence
  outcomes <- unique(outcome.vector)
  outcome.counts <- rep(0, length(outcomes))
  names(outcome.counts) <- as.character(outcomes)
  # count num for each element in vecotr
  for(item in names(outcome.counts)){
    outcome.counts[item] <- sum(outcome.vector == item)
  }
  # calc P
  outcome.counts <- outcome.counts / sum(outcome.counts)
  # return prediction based on max outcome counts
  max.index <- which(outcome.counts == max(outcome.counts))
  return(list('predict' = names(outcome.counts)[max.index][1],
              'margin'  = max(outcome.counts)))
}


# train model
TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  
  #if the data-set is pure, then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  }
  # no attributie can be used for spliting
  else if(ncol(data)==1)
  {
    predict <- predict.outcome(data[,ncol(data)])
    child <- node$AddChild(predict$predict)
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''

  }
  else {
    #chose the feature with the highest information gain
    ig <- sapply(colnames(data)[-ncol(data)], 
                    function(x) InformationGain(
                      table(data[,x], data[,ncol(data)])
                    )
    )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value
      child <- node$AddChild(names(childObs)[i])
      if(class(childObs[[i]])=="factor"){childObs[[i]] <-as.data.frame(childObs[[i]])}
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
  }
}

# prediction
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}


# start to prepare date
bank <- read.table("bank-full.csv", sep=";", header=T)
# bin for age
# banktemp <-bank
# bank <-banktemp

# bank$age <- cut(bank$age, breaks=c(-Inf, 20,30, 40,50,60,Inf), 
#                 labels=c("~20","21~30","31~40","41~50","51~60","6~"))
bank$age <- cut(bank$age, breaks=c(-Inf, 30,55,Inf), 
                labels=c("~30","31~60","55~"))

partition.table = table(bank$age,bank$y)
row.sums = as.vector(rowSums(partition.table))
prob = partition.table / row.sums
mosaicplot(partition.table , shade = T, xlab = "age", ylab = "y", main = "Mosaic Plot")

# bin for balance
# bank <-banktemp

summary(bank$balance)
# bank$balance <- cut(bank$balance, breaks=c(-Inf, 72,1362,1428,Inf), 
#                 labels=c("1","2","3","4"))
bank$balance <- cut(bank$balance, breaks=c(-Inf, 72,Inf),
                    labels=c("<72",">=72"))

partition.table = table(bank$balance,bank$y)
row.sums = as.vector(rowSums(partition.table))
prob = partition.table / row.sums
mosaicplot(partition.table , shade = T, xlab = "balance", ylab = "y", main = "Mosaic Plot")

# bin for pday
table(bank$pdays)  # too many nosie, skip

# start to build tree
# delete unrelated attribute
MyData = subset(bank,select = -c(month,contact,duration,pdays,day,campaign,previous))
# split to training and test data
MyData <- MyData[sample(1:nrow(MyData)),]
MyTest <- MyData[40001:45211,]
MyTrain <- MyData[1:40000,]
library(ROSE)
# oversampling traning data
MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "both",N = 40000, p=0.5, seed=1)$data
# MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "under",N = 20000, seed =1)$data
# MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "both",N = 30000,  p=0.5,seed=1)$data
# cut train and validation data
MyTrain <- MyTrain[sample(1:nrow(MyData)),]
MyValidation <- MyTrain[35000:40000,]
MyValidation <- MyValidation[MyValidation$y %in% c("yes","no"),]
MyTrain <-MyTrain[1:35000,]
MyTrain <- MyTrain[MyTrain$y %in% c("yes","no"),]

# train the tree
tree <- Node$new("bank")
TrainID3(tree, MyTrain)

# predictions <- apply(MyValidation[,1:9],MARGIN = 1,Predict)
# Predict(tree,MyValidation[1,1:9])


print(tree, "feature", "obsCount")
Prune(tree, function(x) x$obsCount> 3000)



 plot(tree)
