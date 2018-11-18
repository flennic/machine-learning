# Special Task 1 
set.seed(12345)
library(readxl)
spambase = read_excel("spambase.xlsx")

knearest = function(data, K = 5, newdata) {
  
  d = function(X, Y) {
    
    X = as.matrix(X)
    Y = as.matrix(Y)
    
    X_hat = X/sqrt(rowSums(X^2))
    Y_hat = Y/sqrt(rowSums(Y^2))
    C = X_hat %*% t(Y_hat)
    D = 1 - C
    
    return(D)  
  }
  
  D = d(data[,-49], newdata[-49])
  
  classification = c()
  
  for (i in 1:nrow(D)) {
    sortedRow = sort(D[,i], index.return = TRUE)
    indexesKnn = sortedRow$ix[1:K]
    classificationRates = newdata$Spam[indexesKnn]
    classification = c(classification, (sum(newdata$Spam[indexesKnn]) > (K/2)))
  }
  
  classifiedCorrectly = sum(classification == newdata$Spam)
  missclassificationRate = 1 - (classifiedCorrectly/nrow(newdata))
  
  return(missclassificationRate)
}

c_data = spambase[1:(nrow(spambase)/2),]
c_newdata = spambase[((nrow(spambase)/2)+1):nrow(spambase),]

#dm = knearest(c_data, K = 30, c_data)
#dm2 = knearest(c_data, K = 30, c_newdata)
#dm2 = knearest(c_data, K = 1, c_data)
#print(dm)
#print(dm2)


density_estimation = function(data, K = 6, X) {
  
  N = length(data)
  S = data
  # V needs to be calculated
  distances = abs(X - S)
  sorted_distances = sort(distances, index.return = TRUE)
  knearest = sorted_distances$x[1:K]
  V = max(knearest) - min(knearest)
  
  return(K/(N*V))
  
  #D = X %*% t(X)
  
  #D = matrix(0, ncol = length(X), nrow = 0)
  
  #for (i in 1:length(X)) {
  #  current_distances = c()
  #  for (j in 1:length(X)) {
  #    current_distances = cbind(current_distances, abs(X[i] - X[j]))
  #  }
  #  D = rbind(D, current_distances)
  #}
  
  #dataSorted = sort(data$speed, index.return = TRUE)
  
  #densities = c()
  
  #for (i in 1:length(X)) {
    
  #}
  
  
}

a = density_estimation(cars$speed, K = 6, X = 3)
print(a)