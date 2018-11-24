# Special Task 1 
set.seed(12345)
library(readxl)
library(ggplot2)
spambase = read_excel("spambase.xlsx")

knearest = function(data, K = 5, newdata) {
  
  d = function(X, Y) {
    
    # Make sure the input in the matrix format
    X = as.matrix(X)
    Y = as.matrix(Y)
    
    # Calculate D as described in the exercise
    X_hat = X/sqrt(rowSums(X^2))
    Y_hat = Y/sqrt(rowSums(Y^2))
    C = X_hat %*% t(Y_hat)
    D = 1 - C
    
    return(D)  
  }
  
  # Get D (trim classification column)
  D = d(data[,-49], newdata[-49])
  
  classificationRate = c()
  classification = c()
  classifiedCorrectly = c()
  
  # For each data entry
  for (i in 1:nrow(D)) {
    # Sort the distances and also get their index
    sortedRow = sort(D[,i], index.return = TRUE)
    
    # Take the K best guys and save their indexed
    indexesKnn = sortedRow$ix[1:K]
    
    # Lookup if they're classified as 0 or 1
    classificationRates = newdata$Spam[indexesKnn]
    
    # Add the classification
    #classification = c(classification, (sum(newdata$Spam[indexesKnn]) > (K/2)))
    classificationRate = c(classificationRate, (sum(newdata$Spam[indexesKnn] / K)))
    temp_classification = sum(newdata$Spam[indexesKnn]) > (K/2)
    classification = c(classification, temp_classification)
    classifiedCorrectly = c(classifiedCorrectly, temp_classification != as.logical(data$Spam[i]))
    }
  
  # Look how many got classified correctly
  #classifiedCorrectly = sum(classification == newdata$Spam)
  
  # Take "1 -" to get the missclassification rate
  #missclassificationRate = 1 - (classifiedCorrectly/nrow(newdata))
  
  returnDataFrame = cbind(newdata, classificationRate)
  returnDataFrame = cbind(returnDataFrame, classification)
  returnDataFrame = cbind(returnDataFrame, classifiedCorrectly)
  
  return(returnDataFrame)
}

#spambase = read_excel("spambase.xlsx")
#c_data = spambase[1:(nrow(spambase)/2),]
#c_newdata = spambase[((nrow(spambase)/2)+1):nrow(spambase),]

# Get the missqualification rates for training and  test
#training_classification = knearest(c_data, K = 30, c_data)
#test_classification = knearest(c_data, K = 30, c_newdata)

# Get the classification rate
#training_rate = sum(training_classification$classification > 0.5) /
#  nrow(training_classification)
#test_rate = sum(test_classification$classification > 0.5) /
#  nrow(test_classification)

# Print the results
#print(head(training_classification[,48:52]))
#print(head(test_classification[,48:52]))

density_estimation = function(data, K = 6, X) {
  
  N = length(data)
  S = data
  # V needs to be calculated
  distances = abs(X - S)
  sorted_distances = sort(distances, index.return = TRUE)
  idx_knearest = sorted_distances$ix[1:K]
  #V = max(data[idx_knearest]) - min(data[idx_knearest])
  V = 2 * abs(max(data[idx_knearest]) - X)
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

print(ggplot(density_data_frame, aes(x = X, y = Density)) + geom_line())

print(ggplot(density_data_frame) +
  geom_line(aes(x = X, y = Density, colour = "Density Function")) +
  labs(title="Density of cars$speed", y="Density", x="X", color = "Legend") +
  scale_color_manual(values = c("orange")))