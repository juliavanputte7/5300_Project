dist_idealv_converter = function(distance=10, time_avg=10, dist_avg=0.25){
  # Read in the data #########
  data <- read_csv("COMBINED_With_Ground_Speed_CLEANED_1Decimal (1).csv") %>%
    setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", ""))
  
  # Filter out NA distances
  data <- data %>% filter(!is.na(distance))
  
  # Goal: Perform moving average, calculate min and max in each window, and connect to make the curve
  # We will use a 30-second time-average to smooth the data a little without losing the shape
  data <- data %>%
    group_by(vehicle, logdate) %>%
    mutate(time_group = floor(second / time_avg) * time_avg) %>%
    ungroup()
  
  # Calculate moving averages of voltage and distance by vehicle and logdate
  data <- data %>%
    group_by(vehicle, logdate, time_group) %>%
    mutate(avg_bmsdcvoltage = mean(bmsdcvoltage, na.rm = TRUE),
           avg_distance = mean(distance, na.rm = TRUE)) %>%
    ungroup()
  
  # Ensure avg_distance_km is present, and bin the distance data into intervals of 0.04 km for each vehicle
  data_binned <- data %>%
    group_by(vehicle, logdate) %>%
    mutate(
      avg_distance_km = distance / 1000  # Convert distance from meters to kilometers
    ) %>%
    ungroup() %>%
    mutate(distance_bin = cut(avg_distance_km, breaks = seq(0, max(avg_distance_km), by = dist_avg), include.lowest = TRUE))
  
  # Step 2: Calculate the average of bmsdcvoltage for each distance bin across all logdates and vehicles
  data_avg <- data_binned %>%
    group_by(vehicle, distance_bin) %>%
    summarize(
      avg_bmsdcvoltage = mean(bmsdcvoltage, na.rm = TRUE),
      avg_distance_km = mean(avg_distance_km)  # To get a representative distance for each bin
    ) %>%
    ungroup()
  
  
  
  # Step 1: Filter the data for the ARG24 vehicle
  data_arg24 <- data_avg %>%
    filter(vehicle == "ARG24")
  
  # Step 2: Find the minimum voltage value for ARG24
  min_voltage_arg24 <- min(data_arg24$avg_bmsdcvoltage, na.rm = TRUE)
  
  # Step 3: Find the corresponding distance where the minimum voltage occurs
  min_distance_arg24 <- data_arg24 %>%
    filter(avg_bmsdcvoltage == min_voltage_arg24) %>%
    pull(avg_distance_km)
  
  # Step 4: Calculate the multiplicative factor to stretch the curve to 20 km
  stretch_factor <- 20 / min_distance_arg24
  
  # Step 5: Apply the factor to stretch the distance values
  data_arg24_stretched <- data_arg24 %>%
    mutate(stretched_distance = avg_distance_km * stretch_factor,
           stretched_voltage = avg_bmsdcvoltage)  # Voltage is unchanged
  
  # Filter the data to only include distances within 0 to 20 km
  data_filtered <- data_arg24_stretched %>% filter(stretched_distance >= 0 & stretched_distance <= 20)
  
  # Find the closest distance to the given distance (10 km)
  idealv = data_filtered %>%
    mutate(distance_diff = abs(stretched_distance - distance)) %>%
    filter(distance_diff == min(distance_diff)) %>%
    pull(stretched_voltage)
  
  return(idealv)
}
