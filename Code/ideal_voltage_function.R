# Define ideal voltage function
ideal_voltage <- function(distances) {
  data_arg24_stretched <- readRDS("Data/data_arg24_stretched.Rds")
  
  # Ensure the distance is within the range of the data
  data <- data_arg24_stretched
  distance_col <- "stretched_distance"
  voltage_col <- "scaled_voltage"
  
  # Define a helper function for a single distance
  get_voltage <- function(distance) {
    if (distance < min(data[[distance_col]])) {
      return(max(data[[voltage_col]]))
    }
    if (distance > max(data[[distance_col]])) {
      return(min(data[[voltage_col]]))
    }
    
    # Interpolate voltage for the given distance
    approx_result <- approx(
      x = data[[distance_col]], 
      y = data[[voltage_col]], 
      xout = distance
    )
    return(approx_result$y)
  }
  
  # Apply the helper function to each element in the vector of distances
  sapply(distances, get_voltage)
}
