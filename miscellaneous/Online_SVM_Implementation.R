# Import Data
spam = read.csv2("spambase.csv")
ind = sample(1:nrow(spam))
spam = spam[ind,c(1:48,58)]

# Set hyper parameters
h = 1 # This is the smoothing parameter for the kernel
beta = 0 # Allowed margin crossing
budget = 50 # Budget (max. amount of support vectors)
training_points = 500 # How many data points are being thrown at our online SVM

gaussian_kernel = function(x, h) {
  return (exp(-x**2)/h)
}

#' predict_using_svm
#'
#' @param svmIdxs Indexes of the support vecot machines 
#' @param xIdx Index of the predicted data point
#' @param data Date.Frame containign the date in rows
#' @param y The column to be predicted
#'
#' @return Returns the predicted value (y) for the new data point
#' @export
#'
#' @examples
predict_using_svm = function(svmIdxs, xIdx, data, y) {
  # We create a matrix where the first row is the new data point and all other
  # rows are the SVMs. We do not inlcude y.
  pre_distance_matrix = rbind(data[xIdx,-y],data[svmIdxs, -y])
  # Now we get the distance from each row compared to the first row, thus the
  # distance from the new data point to all SVMs
  x = gaussian_kernel(as.matrix(dist(pre_distance_matrix,
                                method="euclidean"))[-1, 1], h)
  # Simply adjust to fit c(-1,1)
  # This is special to this exercise
  t = 2 * spam[svmIdxs, y] - 1
  # Multiple them and return the sum, which is the prediction. Don't ask me why
  # take this from the slides
  return(sum(x*t))
}

errors = 1
errorrate = vector(length = training_points)
errorrate[1] = 1
sv = c(1) # In the beginnign we just take the first value as an support vector

# This loop is used to simulate that each data point is given as a stream of data
for(i in 2:training_points) {
  
  # First we predict using the first data point. In a real setup we don't know
  # anything in the beginning and thus would just take the first datapoint
  
  # We get the real and the predicted value
  yi = predict_using_svm(sv, i, spam, 49)
  ti = 2 * spam[i, 49] - 1
  
  # This simply checks if the value is on teh wrong "side", thus if it has been
  # classified correctly
  if(ti * yi < 0) {
    errors = errors + 1
  }
  
  # The new error rate
  errorrate[i] = errors/i
  
  # Verbose
  cat(".") # iteration ", i, "error rate ", errorrate[i], ti * yi, "sv ", length(sv), "\n")
  flush.console()
  
  # So if this new data point didn't cross the border to much (< beta), we consider
  # adding it as a new support vector
  if(ti * yi <= beta) {
    
    # So we add it to the support vectors. In the beginning they will just fill
    # up until they reach the budget
    sv = c(sv, i)
    
    # If the budget has been reached, we have to remove one
    if (length(sv) > training_points) {
      # So now we remove one support vector at a time and see which "models"
      # performs best without that vector. This one we take, so removing this one
      # support vector
      for(m in 1:length(sv)) {
        
        # Support vectors without the current one
        sv2 = sv[-m]
        # Prediction/"Score"
        ym2 = predict_using_svm(sv2, sv[m], spam, 49)
        # Real
        tm = 2 * spam[sv[m],49] - 1
        
        # We take the first "prediction"/"score" no matter what as a candidate
        # for removing
        if(m==1) {
          max = tm * ym2
          ind = 1
        }
        # Now we check if without each support vector the model performs better
        # If yes, we save the new index for removal
        else {
          if(tm * ym2 > max) {
            max = tm * ym2
            ind = m
          }
        }
      }
      # We finally remove the support vector
      sv = sv[-ind]
      
      # cat("removing ", ind, max, "\n")
      # flush.console()
    }
  }
}
plot(errorrate[seq(from=1, to=training_points, by=10)], ylim=c(0.15,0.4), type="o")
training_points
beta
length(sv)
errorrate[training_points]