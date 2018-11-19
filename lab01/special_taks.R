# Special Task 1 
set.seed(12345)
library(readxl)
library(ggplot2)
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
  idx_knearest = sorted_distances$ix[1:K]
  V = max(data[idx_knearest]) - min(data[idx_knearest])
  #V = abs(data[idx_knearest[K]] - data[idx_knearest[1]])
  
  return(K/(N*V))
}

min_speed = min(cars$speed)
max_speed = max(cars$speed)
steps = 1000

x_values = seq(min_speed, max_speed, by = (max_speed - min_speed)/steps)
y_values = unlist(lapply(x_values, function(x) density_estimation(cars$speed, K = 6, x)))

density_data_frame = data.frame(x_values, y_values)
colnames(density_data_frame) = c("X", "Density")

#print(ggplot(density_data_frame, aes(x = X, y = Density)) + geom_line())

print(ggplot(density_data_frame) +
  geom_line(aes(x = X, y = Density, colour = "Density Function")) +
  labs(title="Density of cars$speed", y="Density", x="X", color = "Legend") +
  scale_color_manual(values = c("orange")))