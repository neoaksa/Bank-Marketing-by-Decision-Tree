

library(plyr)
library(igraph)

# this downloads and unzips the dataset
# temp <- tempfile()
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",temp, mode="wb")
# unzip(temp, "bank-full.csv")
# unlink(temp)
# setwd("~/taoj@mail.gvsu.edu/gvsu/course/CIS678/Bank Marketing by Decision Tree/data/bank")
bank <- read.table("bank-full.csv", sep=";", header=T)
str(bank)
# subset for quick testing
bank1 <- bank[1:100,]


# # class example for checking against a known calculation
# wind = c('strong','weak','strong','strong','strong','weak','weak','strong','strong','strong','weak','weak','strong','weak')
# water = c('warm','warm','warm','moderate','cold','cold','cold','moderate','cold','moderate','moderate','moderate','warm','moderate')
# air = c('warm','warm','warm','warm','cool','cool','cool','warm','cool','cool','cool','warm','cool','warm')
# forecast = c('sunny','sunny','cloudy','rainy','rainy','rainy','sunny','sunny','sunny','rainy','sunny','sunny','sunny','rainy')
# oracle = c('yes','no','yes','yes','no','no','no','yes','yes','no','yes','yes','yes','no')
# example = data.frame(wind, water, air, forecast, oracle)


# check for missing values
sum(is.na(bank))


# put duration and pdays into dummy variables
#bank$pdaysBin = as.numeric(bank$pdays <= 374)


# function to calculate entropy
calcEntropy = function(S){
  if(!is.factor(S)) S = as.factor(S)
  
  p = prop.table(table(S))
  
  -sum(sapply(levels(S), function(name) p[name] * log2(p[name])))
}


# function to calculate gain, partition.by is the feature and target is the y
calcGain = function(partition.by, target){
  partition.table = table(partition.by, target)
  
  row.sums = as.vector(rowSums(partition.table))
  
  prob = partition.table / row.sums
  
  ent = -prob * log2(prob)
  
  # define 0 * log2(0) = 0
  ent[is.na(ent)] = 0
  
  info.gain = calcEntropy(target) - sum(ent * (row.sums / sum(row.sums)))
  
  return(info.gain)
}

# get next partition attribute
entropy.partition = function(df, target){
  # extract column names
  cols <- names(df)
  # filter out target
  candidates <- cols[!(cols == target)]
  
  # calculate entropy gain when partitioning target by each candidate
  gain <- NULL
  for(col in candidates){
    gain <- c(gain, calcGain(df[[col]], df[[target]]))
  }
  names(gain) <- candidates
  
  # select the candidate that returns the max information gain
  # in case of a tie, select the first candidate in the list
  max.candidate <- names(which(gain == max(gain))[1])
  
  return(list('max' = max.candidate,
              'info.gain' = gain))
}

# retrun p of majority  in a given vector
predict.outcome <- function(outcome.vector, threshold, majority){
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
  
  # if the prediction, based on max outcome.counts, is greater than the threshold
  # return prediction based on max outcome counts
  # otherwise, go with the simple majority
  if(max(outcome.counts) >= threshold){
    max.index <- which(outcome.counts == max(outcome.counts))
    return(list('predict' = names(outcome.counts)[max.index],
                'margin'  = max(outcome.counts)))
  } else{
    return(majority)
  }
}

# generate a dcision tree
generate.dt <- function(df, target,                 # data frame and classification col
                        threshold, majority,        # dealing with clashes
                        g, parent.id, split.by,     # graph constuction
                        depth){    
  
  # calculate current entropy of target
  current.entropy <- calcEntropy(df[[target]])
  # if entropy is 0 or we're down to the classification column
  if(is.nan(current.entropy)){current.entropy = 0}
  # we are at a leaf. Otherwise we need to branch
  if(current.entropy == 0 | ncol(df) == 1){
    outcome <- predict.outcome(df[[target]], threshold, majority)
    # name of node = classification(s)
    node.name <- paste(outcome$predict, round(outcome$margin, 2), sep="::")
    node.type <- 'leaf'
  } else{
    # determine the next column to split by
    node.name <- entropy.partition(df, target)[[1]]
    node.type <- 'branch'
  }
  
  # add node
  g <- g + vertex(node.name, type=node.type, set.size=nrow(df), depth=depth)
  node.id <- length(V(g))
  # add edge from parent to node (unless parent.id empty)
  if(parent.id != ''){
    # connect parent node with sub notes
    # edge name is the value we are splitting by
    g <- g + edge(c(parent.id, node.id), name=split.by)
  }
  
  # if we're at a branch
  if(node.type == 'branch'){
    # get branch values
    unique.values <- unique(df[[node.name]])
    # filter out the choosen attribute in brach
    col.filter <- !(names(df) %in% node.name)
    split.df <- df[ , col.filter, drop=FALSE]
    # recursively find sub nodes until leaf
    for(value in unique.values){
      new.df <- split.df[df[node.name] == value, , drop=FALSE]
      g <- generate.dt(new.df, target, 
                       threshold, majority,
                       g, node.id, value,
                       depth=depth + 1)
    }
  }
  E(g)$label <- E(g)$name
  return(g)
}

# train a decision tree
train.dt <- function(df, target, threshold){
  # initialize graph
  g <- graph.empty()
  # calculate a simple majority classification rule
  majority <- predict.outcome(df[[target]], 0, list())
  
  # add these as graph attributes
  g$target <- target
  g$threshold <- threshold
  g$majority <- majority
  depth <- 1
  # recursively create a tree
  g <- generate.dt(df, target, 
                   threshold, majority,
                   g, '', '',
                   depth)
  
  V(g)$label <- paste0(V(g)$name, " (", V(g)$set.size, ")")
  return(g)
}


follow.dt <- function(obs, g, current.node){
  # if the curent node is a 'leaf' make a prediction
  # just need to spit out extra support information
  if(V(g)[current.node]$type == 'leaf'){
    return(strsplit(V(g)[current.node]$name, split="::")[[1]][1])
  }
  
  # otherwise, move on to the next node
  
  # get column to split on from name of current node
  split.on <- obs[V(g)[current.node]$name]
  # determine which edge to follow
  for(edge in E(g)[from(current.node)]){
    if(E(g)[edge]$name == split.on){
      # we found the edge we match, follow to next node
      next.node <- V(g)[to(E(g)[edge])]
      return(follow.dt(obs, g, next.node))
    }
  }
}


predict.dt <- function(df, g){
  # initialize output vector
  outcome.vector <- c()
  
  # start at the root node
  root.node <- 1
  
  # for each row (observation) in the dataset, use g to make a prediction
  for(n in 1:nrow(df)){
    obs <- df[n, ]
    outcome.vector <- c(outcome.vector, follow.dt(obs, g, root.node))
  }
  return(outcome.vector)
}

# create a path from root to spec node
path.constructor <- function(g, target.node){
  # get the edges from the root to the target node
  p <- get.shortest.paths(g, from=1, to=target.node, output='epath')
  
  # initialize return value
  path.list <- list()
  
  # for each edge in path, add to path.list c(origin node name, edge name)
  for(e in p$epath[[1]]){
    # end points of edge: [1] towared root, [2] away from root
    end.points <- get.edge(g, e)
    path.list <- c(path.list, list(c( V(g)[end.points[1]]$name, E(g)[e]$name )))
  }
  return(path.list)
}


node.filter <- function(g, target.node){
  df.filter <- paste(lapply(path.constructor(g, target.node),
                            function(i) {paste0(i[1],
                                                " == ",
                                                "'", i[2], "'")}),
                     collapse=" & ")
  return(df.filter)
}


fast.predict <- function(df, g){
  df$prediction.column <- rep(NA, nrow(df))
  for(leaf in V(g)[V(g)$type == 'leaf']){
    predict <- strsplit(V(g)[leaf]$name, split="::")[[1]][1]
    f <- node.filter(g, leaf)
    
    attach(df)
    df[eval(parse(text = f)), 'prediction.column'] <- predict
    detach(df)
  }
  return(df$prediction.column)
}

# calculate prune.static.error and
# prune.weight for all nodes in graph
prune.tree <- function(g, df){  
  for(ver in V(g)){
    f <- node.filter(g, ver)
    if(f == ""){
      node.df <- df
    } else{
      # use parse to change f to expression
      node.df <- with(df, df[eval(parse(text = f)), ])
    }
    
    V(g)[ver]$prune.weight <- nrow(node.df)
    
    node.prediction <- predict.outcome(node.df[[g$target]], 
                                       g$threshold, g$majority)$predict
    st.err <- sum(node.prediction != node.df[[g$target]]) / V(g)[ver]$prune.weight
    
    V(g)[ver]$prune.static.error <- st.err
    V(g)[ver]$prune.predict <- node.prediction
  }
  
  # get all pruning candidates
  leaf.parents <- V(g)[ nei( V(g)[which(V(g)$type == 'leaf')] ) ]
  
  prune.targets <- c()
  for(node in leaf.parents){
    ds.neighbors <- V(g)[ nei(node, mode='out')]
    if(all(V(g)[ nei(node, mode='out')]$type == 'leaf')){
      prune.targets <- c(prune.targets, node)
    }
  }
  
  # for each pruning candidate
  for(n in prune.targets){
    # calculate backed-up error
    bkd.up.err <- 0
    for(c in neighbors(g, n, mode='out')){
      bkd.up.err <- bkd.up.err + (V(g)[c]$prune.weight * V(g)[c]$prune.static.error)
    }
    bkd.up.err <- bkd.up.err / V(g)[n]$prune.weight
    
    # if static error < backed-up error prune
    if(V(g)[n]$prune.static.error < bkd.up.err){
      # prune
      # delete leaves
      delete.vertices(g, neighbors(g, n, mode='out'))
      
      # Create new leaf
      V(g)[n]$type <- 'leaf'
      V(g)[n]$name <- paste(V(g)[n]$prune.predict, 
                            round(V(g)[n]$prune.static.error, 2), sep="::")
    }
  }
  return(g)
}


prune.dt <- function(g, df){
  delta <- 1
  while(delta > 0){
    starting.num <- vcount(g)
    g <- prune.tree(g, df)
    delta <- starting.num - vcount(g)
  }
  return(g)
}


pretty.plot <- function(g){
  layout <- layout.reingold.tilford(g)
  # rotate
  layout <- layout[ , c(2, 1)]
  # flip
  layout[ ,1] <- max(layout[ ,1]) - layout[ ,1, drop=FALSE]
  # stretch
  layout[ ,1] <- layout[ ,1] * layout[ ,1]
  
  plot(g, layout=layout, asp=0,
       vertex.shape='rectangle', vertex.size=20, vertex.size2=10,
       vertex.color='white', vertex.frame.color='white',
       edge.label.color='red', edge.arrow.size=0.3)
}

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
MyValidation <- MyTrain[50000:60000,]
MyValidation <- MyValidation[MyValidation$y %in% c("yes","no"),]
MyTrain <-MyTrain[1:50000,]
MyTrain <- MyTrain[MyTrain$y %in% c("yes","no"),]
# build decision tree
tree.training <- train.dt(MyTrain,"y",0.5)

prune.dt(tree.training,MyTest)
pretty.plot(tree.training)

tree.training[V(tree.training)$depth<=4]

c <- tree.training
c <- delete.vertices(c,V(tree.training)$depth<=4)
c<-V(tree.training)[(V(tree.training)$depth<=4)]


delete.vertices(c,V(c)[V(tree.training)$depth<=3])
c <- induced_subgraph(tree.training, V(tree.training)[(V(tree.training)$depth<=3)], 
                      impl =  "copy_and_delete")
pretty.plot(c)


table(MyTrain$y)
unique(MyTrain$campaign)
unique(MyTrain$previous)
levels(MyTrain$job)
# ID3 function
#ID3 = function(attributes, target){
#  # check if all examples are of same class
#  if(length(unique(target))<=1){
#    leaf = names(which.max(table(target)))
#  }
#  # if there are no more attributes to test
#  if(length(attributes) <= 0){
#    leaf = names(which.max(table(target)))
#  }
#  else {
#    # find the attribute with highest gain
#    max_gain = 0
#    for(attr in attributes){
#      new_gain = calcGain(attr, target)
#      if(new_gain > max_gain){
#        max_gain = new_gain
#        best_attr = attr
#      }
#    }
#  }
#}