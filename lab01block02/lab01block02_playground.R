# Assignment 2
set.seed(1234567890)

# max number of EM iterations
max_it = 1

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

pbern = function(x, p) {
  return(p * (1-p)^(1-x))
}

initialize_gamma = function(x, mu, pi) {
  
  l_x = x
  l_mu = mu
  l_pi = pi
  
  gamma = function(n, k) {
    
    X = x[n,]
    
    ##### Nominator
    bernoulli_x_given_mu = 1
    
    for (i in 1:ncol(l_mu)) {
      bernoulli_x_given_mu = bernoulli_x_given_mu * pbern(X[i], mu[k, i])
    }
    
    p_given_x_nom = pi[k] * bernoulli_x_given_mu
    
    ##### Denominator
    denominator_sum = 0
    
    for (j in 1:nrow(l_mu)) {
      ## First Bernoulli
      
      bernoulli_x_given_mu_denom = 1
      
      for (l in 1:ncol(l_mu)) {
        bernoulli_x_given_mu_denom = bernoulli_x_given_mu_denom * pbern(X[l], mu[j, l])
      }
      
      p_given_x_denom = pi[j] * bernoulli_x_given_mu_denom
      denominator_sum = denominator_sum + p_given_x_denom
    }
    
    return(p_given_x_nom/denominator_sum)
  }
   
  return(gamma)
}


gamma_calc = initialize_gamma(x, mu, pi)

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # The E-step estimates the probability of each point belonging to each cluster
  # Your code here
  for (l_n in 1:N) {
    for (l_k in 1:K) {
      z[l_n, l_k] = gamma_calc(l_n, l_k)
    }
  }
  
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















