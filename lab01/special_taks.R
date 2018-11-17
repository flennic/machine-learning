# Special Task 1 
set.seed(12345)
library(readxl)
spambase = read_excel("spambase.xlsx")

knearest = function(data, K = 5, newdata) {
  
  ## Get a matrix with all the distances
  #D = d(data[, -49], newdata[, -49])
  D = d(data, newdata)
  
  classification = c()
  
  for (i in 1:nrow(D)) {
    sortedRow = sort(D[,i], index.return = TRUE)
    indexesKnn = sortedRow$ix[2:(K+1)] ## First one is the distance to the point itself
    classificationRates = newdata$Spam[indexesKnn]
    classification = c(classification, (sum(newdata$Spam[indexesKnn]) > (K/2)))
  }
  
 classifiedCorrectly = sum(classification == newdata$Spam)
 missclassificationRate = 1 - (classifiedCorrectly/nrow(newdata))

  ## Add indexes
  
  
  ## Sort it
  #rownames(D) = 1:nrow(D)
  #colnames(D) = 1:ncol(D)
  #D_sorted = t(apply(t(D), 1, function(x) sort(x, index.return=TRUE)))
  
  ## Slice the first entry as it is the distance to teh point itself
  #D_sorted = D_sorted[, -1]
  
  ## Take the 5 closest ones
 # D_sorted_sliced = D_sorted[, 1:K]
  
  ## Do the calulcations
  
  ## Do the classification of the newdata
  
  return(missclassificationRate)
}

d = function(X, Y) {

  X = as.matrix(X)
  Y = as.matrix(Y)
  
  X_hat = X/sqrt(rowSums(X^2))
  Y_hat = Y/sqrt(rowSums(Y^2))
  C = X_hat %*% t(Y_hat)
  D = 1 - C
  
  # = 1 - ((X %*% t(Y)) /(sqrt(rowSums(X^2) %*% t(rowSums(Y^2)))))
  
  #X_hat = X/sqrt(rowSums(X * X))
  #Y_hat = Y/sqrt(rowSums(Y * Y))
  
  #C = X_hat %*% t(Y_hat)
  #D = as.distance(1 - C)
  
  return(D)  
}

#a = matrix(c(1, 1, -1, 1), nrow = 2)
#b = matrix(c(1, 1, -1, -1), nrow = 2)
#c = d(a,b)
#print(c)

c_data = spambase[1:(nrow(spambase)/2),]
c_newdata = spambase[((nrow(spambase)/2)+1):nrow(spambase),]

dm = knearest(c_data, K = 30, c_newdata)
print(dm)