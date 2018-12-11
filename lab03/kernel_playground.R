kernel_gauss_distance = function(pointA, pointB, smoothing) {
  # Use distHaversine() as a help
  # Needed as distHaversine does not operate on dataframes with more than two
  # columns
  m = cbind(pointB$latitude, pointB$longitude)
  u = distHaversine(m, c(pointA$latitude, pointA$longitude))
  u = u / smoothing
  return(exp(-(u^2)))
}

kernel_gauss_day = function(dayA, dayB, smoothing) {
  u = as.numeric(as.Date(dayA$date)-as.Date(dayB$date))
  u = u / smoothing
  return(exp(-(u^2)))
}

kernel_gauss_hour = function (hourA, hourB, smoothing) {
  u = as.numeric(difftime(strptime(hourA$time, format = "%H:%M:%S"),
                          strptime(hourB$time, format = "%H:%M:%S")))
  u = u / smoothing
  return(exp(-(u^2)))
}

kernel_sum = function(A, B, h_distance, h_date, h_time) {
  return(
    kernel_gauss_distance(A, B, h_distance) + 
      kernel_gauss_day(A, B, h_date) + 
      kernel_gauss_hour(A, B, h_time)
  )
}

kernel_product = function(A, B, h_distance, h_date, h_time) {
  return(
    kernel_gauss_distance(A, B, h_distance) * 
      kernel_gauss_day(A, B, h_date) * 
      kernel_gauss_hour(A, B, h_time)
  )
}

predict_weather = function(u_latitude, u_longtitude, u_date, u_type) {
  
  # Data
  stations = read.csv("stations.csv", encoding = "UTF-8")
  temps = read.csv("temps50k.csv", encoding = "UTF-8")
  st = merge(stations, temps, by="station_number")
  
  # Filter all date posterior to the given date (doesn't make sense to predict
  # something that we already now)
  st = st[as.Date(st$date) < as.Date(u_date),]
  
  # Given Data
  # Each user input should be a list of:
  # latitude, longitude, date, time
  # time is created with the loop at we predict for every time for the given day
  
  # Times to predict
  times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00",
            "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00",
            "24:00:00")
  # Temperatures to predict
  temp = vector(length=length(times))
  
  # Smoothing factors
  h_distance = 10
  h_date = 10
  h_time = 10
  
  # Prediction for each temp to predict
  for (i in 1:length(times)) {
    
    # User data point for this iteration
    user_data_point = list(
      latitude = u_latitude,
      longitude = u_longtitude,
      date = u_date,
      time = times[i])
    
    if (u_type == "sum") {
      # Parameters: A, B, h_distance, h_date, h_time
      k_sum = kernel_sum(user_data_point, st, h_distance, h_distance, h_time)
      # Now that we have the kernel value, we can calcuate the actual prediction
      # Formula taken from slide 8 from 
      # "Histogram, Moving Window, and Kernel Regression""
      y = k_sum %*% st$air_temperature/sum(k_sum)
    }
    else if (u_type == "prod") {
      # Parameters: A, B, h_distance, h_date, h_time
      k_prod =
        kernel_product(user_data_point, st, h_distance, h_distance, h_time)
    }
    else {
      stop("Du Nasenbär!")
    }
    
    ## Calculate the kernel sum and product
    
    
    
  }
  
  return("lala")
}

set.seed(1234567890)

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)

# Students' code here

predict_weather(a, b, date, "sum")

plot(temp, type="o")