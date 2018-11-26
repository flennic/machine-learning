# Assignment 2
set.seed(1234567890)

# max number of EM iterations
max_it = 100

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
  return(p^x * (1-p)^(1-x))
}

bernoulli = function(X_f, mu_f) {
  
  bernulli_product = 1
  
  for (i in 1:length(mu_f)) {
    bernulli_product = bernulli_product * pbern(X_f[i], mu_f[i])
  }
  return(bernulli_product)
}

old_likelihood = -Inf

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  #Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # The E-step estimates the probability of each point belonging to each cluster
  # Your code here
  
  gamma = function(n, k) {
    
    ## Nominator
    nominator = pi[k] * bernoulli(x[n,], mu[k,])
    
    ## Denominator
    denominator = 0
    for (l_k in 1:K) {
      denominator = denominator + pi[l_k] * bernoulli(x[n,], mu[l_k,])
    }
    
    #X = x[n,]
    
    ##### Nominator
    #bernoulli_x_given_mu = bernoulli(X_f = X, mu_f = mu[k,])
    
    #p_given_x_nom = pi[k] * bernoulli_x_given_mu
    
    ##### Denominator
    #denominator_sum = 0
    
    #for (j in 1:nrow(mu)) {
      
    #  ## First Bernoulli
    #  bernoulli_x_given_mu_denom = bernoulli(X_f = X, mu_f = mu[j,])
    #  p_given_x_denom = pi[j] * bernoulli_x_given_mu_denom
    #  denominator_sum = denominator_sum + p_given_x_denom
    #}
    
    return(nominator/denominator)
  }
  
  for (l_n in 1:N) {
    for (l_k in 1:K) {
      z[l_n, l_k] = gamma(l_n, l_k)
    }
  }
  
  #Log likelihood computation.
  # Your code here
  
  current_likelihood = 0
  
  for (i in 1:N) {
    k_sum = 0
    for (l_k in 1:K) {
      current_val = pi[l_k] * bernoulli(X_f = x[i,], mu_f = mu[l_k,])
      k_sum = k_sum + current_val
    }
    
    ln_sum = log(k_sum)
    current_likelihood = current_likelihood + ln_sum
  }

  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  
  #if (it > 1 && abs(llik[it-1] - current_likelihood) < min_change) break
  
  llik[it] = current_likelihood
 
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
  ### mu new
  for (l_k in 1:K) {
    for (d in 1:D) {
      
      sum_nominator = 0
      sum_denominator = 0
      
      for (l_n in 1:N) {
        ## Nominator
        sum_nominator = sum_nominator + z[l_n, l_k] * x[l_n, d]
        
        
        
        ## Denominator
        sum_denominator = sum_denominator + z[l_n, l_k]
        
      }
      
      mu[l_k, d] = sum_nominator / sum_denominator
    }
  }
  
  ### pi new
  for (k_l in 1:K) {
    
    pi_new_sum = 0
    
    ## Nominator
    for (n_l in 1:N) {
      pi_new_sum = pi_new_sum + z[n_l, k_l]
    }
    ## Denominator (is just N)
    pi[k_l] = pi_new_sum / N
  }
}

pi
mu
plot(llik[1:it], type="o")















