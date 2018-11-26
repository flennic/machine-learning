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
  geom_point(aes(x = n, y = error_rate_training), colour = "red") +
  geom_line(aes(x = n, y = error_rate_validation, colour = "
                AdaBoost Validation")) +
  geom_point(aes(x = n, y = error_rate_validation), colour = "orange") +
  geom_line(aes(x = n, y = error_rate_training, colour = "Random Forest Training"), data = rf_errors, linetype = "dashed") +
  geom_point(aes(x = n, y = error_rate_training), colour = "blue", data = rf_errors) +
  geom_line(aes(x = n, y = error_rate_validation, colour = "Random Forest Validation"), data = rf_errors) +
  geom_point(aes(x = n, y = error_rate_validation), colour = "steelblue2", data = rf_errors) +
  labs(title = "Random Forest and AdaBoost", y = "Error Rate", x = "Number of Forests", color = "Legend") +
  scale_color_manual(values = c("red", "orange", "blue", "steelblue2"))


# Assignment 2
set.seed(1234567890)

# max number of EM iterations
max_it = 10

# min change in log likelihood between two consecutive EM iterations
min_change = 0.1 

# number of training points
N = 1000 

# number of dimensions
D = 10

# training data
x = matrix(nrow=N, ncol=D) 

# true mixing coefficients
true_pi = vector(length = 3) 

# true conditional distributions
true_mu = matrix(nrow=3, ncol=D)

true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)

plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))

points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")

# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}

# number of guessed components
K = 3 

# fractional component assignments
z = matrix(nrow=N, ncol=K) 

# mixing coefficients
pi = vector(length = K)

# conditional distributions
mu = matrix(nrow=K, ncol=D) 

# log likelihood of the EM iterations
llik = vector(length = max_it)

# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

pi
mu

initialize_gamma = function(x, mu, pi) {
  
  l_x = x
  l_lmu = mu
  l_pi = pi
  
  gamma = function(n, k, d) {
    
    x_n = sum(x[1,]/length(x[1,]))
    
    nominator = pi[k] * pnorm(q = x_n, mean = mu[k, d])
    
    denominator = 0
    
    for (j in 1:k) {
      denominator = denominator + (pi[j] * pnorm(q = x_n, mean = mu[j, d]))
    }
    
    return(nominator/denominator)
  }
  
  return(gamma)
}

gamma_of_z = function(n, k, x, mu, pi, mu_idx, pi_idx) {
  nominator = pi * pnorm(q = x, mean = mu_idx)
  
  denominator = 0
  
  for (j in 1:k) {
    denominator = denominator + (pi[j] * pnorm(q = x, mean = mu[j]))
  }
  
  return(nominator/denominator)
}

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # The E-step estimates the probability of each point belonging to each cluster
  # Your code here
  #pnorm(q = x, mean = mu)
  
  #Log likelihood computation.
  # Your code here
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
  
}

pi
mu
plot(llik[1:it], type="o")























