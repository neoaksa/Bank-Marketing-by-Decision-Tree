# class example for checking against a known calculation
wind = c('strong','weak','strong','strong','strong','weak','weak','strong','strong','strong','weak','weak','strong','weak')
water = c('warm','warm','warm','moderate','cold','cold','cold','moderate','cold','moderate','moderate','moderate','warm','moderate')
air = c('warm','warm','warm','warm','cool','cool','cool','warm','cool','cool','cool','warm','cool','warm')
forecast = c('sunny','sunny','cloudy','rainy','rainy','rainy','sunny','sunny','sunny','rainy','sunny','sunny','sunny','rainy')
oracle = c('yes','no','yes','yes','no','no','no','yes','yes','no','yes','yes','yes','no')
example = data.frame(wind, water, air, forecast, oracle)
# function to calculate entropy
calcEntropy = function(S){
  if(!is.factor(S)) S = as.factor(S)
  
  p = prop.table(table(S))
  
  -sum(sapply(levels(S), function(name) p[name] * log2(p[name])))
}
target = 'oracle'

current.entropy = calcEntropy(example[[target]])
if(current.entropy == 0 | ncol(example) == 1){
}

# function to calculate entropy
calcEntropy = function(S){
  if(!is.factor(S)) S = as.factor(S)
  
  p = prop.table(table(S))
  
  -sum(sapply(levels(S), function(name) p[name] * log2(p[name])))
}

S=c(1,3,2,33,441,1,2,2,2)
S=as.factor(S)
T=table(S)
levels(S)
prop.table(T)
if(!is.factor(S)) S = as.factor(S)

p = prop.table(table(S))

-sum(sapply(levels(S), function(name) p[name] * log2(p[name])))

m <- matrix(1:4, 2)
m
prop.table(m, 1)

