# Special Task 1 
set.seed(12345)
spambase = read_excel("./spambase.xlsx")

knearest = function(data, K = 5, newdata) {
  
  X_data = data[,47]
  Y_data = data[,48]
  
  ## Get a matrix with all the distances
  D = d(X_data, Y_data)
  
  ## Sort it
  
  ## Take the 5 closest ones
  
  ## Do the calulcations
  
  ## Do the classification of the newdata
  
  return(D)
}

d = function(X, Y) {
  
  X = as.matrix(X)
  Y = as.matrix(Y)
  
  X_hat = X/sqrt(rowSums(X*X))
  Y_hat = Y/sqrt(rowSums(Y*Y))
  C = X_hat %*% t(Y_hat)
  D = 1 - C
  
  return(D)
}

a = matrix(c(1, 1, -1, 1), nrow = 2)
b = matrix(c(1, 1, -1, -1), nrow = 2)
c = d(a,b)
print(c)

#c_data = spambase[1:(nrow(spambase)/2),]
#c_newdate = spambase[((nrow(spambase)/2)+1):nrow(spambase),]

#dm = knearest(c_data, K = 5, c_newdate)