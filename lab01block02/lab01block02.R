# Assignment 1

library(mboost)
library(randomForest)
library(ggplot2)

set.seed(12345)

spambase = read.csv("spambase.csv", sep=";", dec = ",")
spambase$Spam = as.factor(spambase$Spam)

n = dim(spambase)[1]
id = sample(1:n, floor(n*0.67))
train_spambase = spambase[id,]
val_spambase = spambase[-id,]


# General Information
c_formula = Spam ~ .
tree_sizes = seq(from = 10, to = 100, by = 10)

# Random Forest
rf_errors = data.frame(n = numeric(), error_rate_training = numeric(),
                       error_rate_validation = numeric())

for (i in tree_sizes) {
  
  # Create the forest
  c_randomForest = randomForest(formula = c_formula, data = train_spambase, ntree = i)
  
  # Do the prediction on the validation dataset
  c_prediction_training = predict(object = c_randomForest, newdata = train_spambase)
  c_prediction_validation = predict(object = c_randomForest, newdata = val_spambase)
  
  # Get the error rate
  c_error_rate_training = 1 - sum(c_prediction_training == train_spambase$Spam)/nrow(train_spambase)
  c_error_rate_validation = 1 - sum(c_prediction_validation == val_spambase$Spam)/nrow(val_spambase)
  
  rf_errors = rbind(rf_errors,
                    list(n = i,
                         error_rate_training = c_error_rate_training,
                         error_rate_validation = c_error_rate_validation))
}

print(rf_errors)

# AdaBoost
adb_errors = data.frame(n = numeric(), error_rate_training = numeric(),
                       error_rate_validation = numeric())

for (i in tree_sizes) {

  # Create the model
  c_adaBoost = blackboost(formula = c_formula, data = train_spambase, family = AdaExp(), control=boost_control(mstop=i))
  
  # Do the prediction on the validation dataset
  c_prediction_training = predict(object = c_adaBoost, newdata = train_spambase, type = "class")
  c_prediction_validation = predict(object = c_adaBoost, newdata = val_spambase, type = "class")
  
  # Get the error rate
  c_error_rate_training = 1 - sum(c_prediction_training == train_spambase$Spam)/nrow(train_spambase)
  c_error_rate_validation = 1 - sum(c_prediction_validation == val_spambase$Spam)/nrow(val_spambase)
  
  adb_errors = rbind(adb_errors,
                    list(n = i,
                         error_rate_training = c_error_rate_training,
                         error_rate_validation = c_error_rate_validation))
}

print(adb_errors)

ggplot(adb_errors) +
  geom_line(aes(x = n, y = error_rate_training, colour = "AdaBoost Training"), linetype = "dashed") +
  geom_point(aes(x = n, y = error_rate_training), colour = "orange") +
  geom_line(aes(x = n, y = error_rate_validation, colour = "AdaBoost Validation")) +
  geom_point(aes(x = n, y = error_rate_validation), colour = "red") +
  geom_line(aes(x = n, y = error_rate_training, colour = "Random Forest Training"), data = rf_errors, linetype = "dashed") +
  geom_point(aes(x = n, y = error_rate_training), colour = "blue", data = rf_errors) +
  geom_line(aes(x = n, y = error_rate_validation, colour = "Random Forest Validation"), data = rf_errors) +
  geom_point(aes(x = n, y = error_rate_validation), colour = "steelblue2", data = rf_errors) +
  labs(title = "Random Forest and AdaBoost", y = "Error Rate", x = "Number of Forests", color = "Legend") +
  scale_color_manual(values = c("red", "orange", "blue", "steelblue2"))
