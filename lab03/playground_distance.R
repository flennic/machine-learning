library(geosphere)

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