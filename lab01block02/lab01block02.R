# Assignment 1

library(mboost)
library(randomForest)
library(ggplot2)

set.seed(1234567890)

spambase = read.csv("spambase.csv", sep=";", dec = ",")
spambase$Spam = as.factor(spambase$Spam)

n = dim(spambase)[1]
id = sample(1:n, floor(n*0.67))
train_spambase = spambase[id,]
val_spambase = spambase[-id,]

# AdaBoost



# Random Forest
rf_errors = data.frame(n = numeric(), error_rate_training = numeric(),
                       error_rate_validation = numeric())

tree_sizes = seq(from = 10, to = 100, by = 10)

for (i in tree_sizes) {
  # Create the forest
  c_randomForest = randomForest(formula = Spam ~ ., data = train_spambase, ntree = i)
  
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

ggplot(rf_errors) +
  geom_line(aes(x = n, y = error_rate_training, colour = "Training")) +
  geom_line(aes(x = n, y = error_rate_validation, colour = "Validation")) +
  labs(title="Random Forest", y="Error Rate", x="Number of Forests", color = "Legend") +
  scale_color_manual(values = c("blue", "orange"))


