library(geosphere)

min_distance = function(a, b, ringSize) {
  
  boundOne = sapply(b, FUN = function(x) abs(a - x))
  boundTwo = sapply(b, FUN = function(x) abs(a+ringSize-x))
  
  return(pmin(boundOne, boundTwo))
}

kernel_gauss_distance = function(pointA, pointB, smoothing) {
  # Use distHaversine() as a help
  # Needed as distHaversine does not operate on dataframes with more than two
  # columns
  # Smooting factor should be > 10000
  m = cbind(pointB$longitude, pointB$latitude)
  u = distHaversine(m, c(pointA$longitude, pointA$latitude))
  u = u / smoothing
  return(exp(-(u^2)))
}

kernel_gauss_day = function(dayA, dayB, smoothing) {
  #u = abs(as.numeric(as.Date(dayA$date)-as.Date(dayB$date)))
  #u = (as.numeric(as.Date(dayA$date)) %% 365) +
  #  (as.numeric(as.Date(dayB$date)) %% 365)
  # We need a new line for this as R is TO FUCKING STUPID to calculate 367 %%
  # 365. R thinks, it's 367. May god have mercy with this language.
  #u = u %% 365
  
  dayA_in_days = as.numeric(strftime(as.Date(dayA$date), '%j'))
  dayB_in_days = as.numeric(strftime(as.Date(dayB$date), '%j'))
  
  u = min_distance(dayA_in_days, dayB_in_days, 365)
  u = u / smoothing
  return(exp(-(u^2)))
}

kernel_gauss_hour = function (hourA, hourB, smoothing) {
  #u = abs(as.numeric(difftime(strptime(hourA$time, format = "%H:%M:%S"),
  #                        strptime(hourB$time, format = "%H:%M:%S"))))
  
  #hourA_in_h = as.numeric(difftime(strptime(hourA$time, format = "%H:%M:%S"),
  #                                 strptime("00:00:00", format = "%H:%M:%S")))
  #hourB_in_h = as.numeric(difftime(strptime(hourB$time, format = "%H:%M:%S"),
  #                                 strptime("00:00:00", format = "%H:%M:%S")))
  hourA_in_h = sapply(hourA$time, FUN = function(x)
    as.numeric(difftime(strptime(x, format = "%H:%M:%S"),
                        strptime("00:00:00", format = "%H:%M:%S"))))
  
  hourB_in_h = sapply(hourB$time, FUN = function(x)
    as.numeric(difftime(strptime(x, format = "%H:%M:%S"),
                        strptime("00:00:00", format = "%H:%M:%S"))))
  
  #u = sapply()
  
  u = min_distance(hourA_in_h, hourB_in_h, 24)
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
  h_distance = 5000
  h_date = 7
  h_time = 0.5
  
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
      kernel = kernel_sum(user_data_point, st, h_distance, h_date, h_time)
    }
    else if (u_type == "prod") {
      # Parameters: A, B, h_distance, h_date, h_time
      kernel =
        kernel_product(user_data_point, st, h_distance, h_date, h_time)
    }
    else {
      stop("Du Nasenbaer!")
    }
    
    # Now that we have the kernel value, we can calcuate the actual prediction
    # Formula taken from slide 8 from 
    # "Histogram, Moving Window, and Kernel Regression""
    y = kernel %*% st$air_temperature/sum(kernel)
    
    # Let's save this predicted temperature
    temp[i] = y
  }
  
  return(data.frame(times, temp))
}

set.seed(1234567890)

# Linkoeping
latitude = 58.410809
longitude = 15.621373

# Kiruna
b_latitude = 67.8558
b_longitude = 20.2253

#a <- 17.6935 #58.4274 # The point to predict (up to the students)
#b <- 59.9953 #14.826
date <- "2000-05-08" # The date to predict (up to the students)

# Students' code here

pred_lin = predict_weather(latitude, longitude, date, "prod")
pred_lin2 = predict_weather(latitude, longitude, date, "sum")
pred_kir = predict_weather(b_latitude, b_longitude, date, "prod")
pred_kir2 = predict_weather(b_latitude, b_longitude, date, "sum")

#plot(temp, type="o")