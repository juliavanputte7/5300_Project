# Read in the data #########
data <- read_csv("Data/COMBINED_With_Ground_Speed_CLEANED_1Decimal (1).csv") %>%
  setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", ""))

# Filter out NA distances
data <- data %>% filter(!is.na(distance))

# Goal: Perform moving average, calculate min and max in each window, and connect to make the curve
# We will use a 30-second time-average to smooth the data a little without losing the shape
data <- data %>%
  group_by(logdate) %>%
  mutate(time_group = floor(second / 30) * 30) %>%
  ungroup()

# Calculate moving averages of voltage and distance by vehicle and logdate
data <- data %>%
  group_by(logdate, time_group) %>%
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
  mutate(distance_bin = cut(avg_distance_km, breaks = seq(0, max(avg_distance_km), by = 1), include.lowest = TRUE))

# Step 2: Calculate the average of bmsdcvoltage for each distance bin across all logdates and vehicles
data_avg <- data_binned %>%
  group_by(vehicle, distance_bin) %>%
  summarize(
    avg_bmsdcvoltage = mean(bmsdcvoltage, na.rm = TRUE),
    avg_distance_km = min(avg_distance_km)  # To get a representative distance for each bin
  ) %>%
  ungroup()



# Step 1: Filter the data for the ARG24 vehicle
# CHANGE THIS IF YOU WANT TO DO IT FOR ARG25
# ARG23 DATA WAS BAD
data_arg24 <- data_avg %>%
  filter(vehicle == "ARG24")

# Step 2: Find the minimum voltage value for ARG24
min_voltage_arg24 <- min(data_arg24$avg_bmsdcvoltage, na.rm = TRUE)
max_voltage_arg24 <- max(data_arg24$avg_bmsdcvoltage, na.rm = TRUE)

# Step 2.5: find the stretch factor for the voltage!
# Find the minimum and maximum voltage value for ARG24 (not averaged)
# Filter the data for the ARG24 vehicle
data_arg24_noavg <- data %>%
  filter(vehicle == "ARG24") %>% filter(distance <= 22000)

# Find the minimum voltage value for ARG24 (average grouped by 30 seconds since this is what we use in our regression model)
min_voltage_arg24_noavg <- min(data_arg24_noavg$avg_bmsdcvoltage, na.rm = TRUE)
max_voltage_arg24_noavg <- max(data_arg24_noavg$avg_bmsdcvoltage, na.rm = TRUE)

# Step 3: Find the corresponding distance where the minimum voltage occurs
min_distance_arg24 <- data_arg24 %>%
  filter(avg_bmsdcvoltage == min_voltage_arg24) %>%
  pull(avg_distance_km)

# Step 4: Calculate the multiplicative factor to stretch the curve to 20 km
stretch_factor <- 22 / min_distance_arg24

# Step 5: Apply the factor to stretch the distance and voltage values
data_arg24_stretched <- data_arg24 %>%
  mutate(
    stretched_distance = avg_distance_km * stretch_factor,
    stretched_voltage = avg_bmsdcvoltage,  # Original voltage before scaling
    # Scale the voltage
    scaled_voltage = min_voltage_arg24_noavg + 
      ((stretched_voltage - min_voltage_arg24) / 
         (max_voltage_arg24 - min_voltage_arg24)) * 
      (max_voltage_arg24_noavg - min_voltage_arg24_noavg)
  ) %>%
  filter(stretched_distance <= 22)


ggplot(data_arg24_stretched, aes(x = stretched_distance, y = scaled_voltage)) +
  geom_line(size = 1, color = "blue") +  # Plot the line for average BMS DC Voltage
  geom_hline(yintercept = max_voltage_arg24_noavg, color="blue",linetype = "dashed") + # Plot line for maximum voltage
  geom_hline(yintercept = min_voltage_arg24_noavg, color="blue",linetype = "dashed") + # Plot line for minimum voltage
  labs(
    title = "Ideal BMS DC Voltage Over Distance (Up to 22 km)",
    x = "Distance (km) (Stretched to 22 km)",
    y = "Ideal BMS DC Voltage"
  ) +
  xlim(0, 22) +  # Limit the x-axis to 22 km
  theme_minimal()+
  # Add ticks and labels for min and max voltage
  annotate("text", x = 1, y = min_voltage_arg24_noavg, label = paste("Min Voltage"), 
           vjust = -0.5, color = "blue", size = 3) +
  annotate("text", x = 1, y = max_voltage_arg24_noavg, label = paste("Max Voltage"),
           vjust = -0.5, color = "blue", size = 3)

saveRDS(data_arg24_stretched, file = "Data/data_arg24_stretched.Rds")

# Define ideal voltage function
ideal_voltage <- function(distance) {
  data_arg24_stretched <- readRDS("Data/data_arg24_stretched.Rds")
  # Ensure the distance is within the range of the data
  data = data_arg24_stretched
  distance_col = "stretched_distance"
  voltage_col = "scaled_voltage"
  if (distance < min(data[[distance_col]]))  {
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
  
  # Return the interpolated voltage value
  return(approx_result$y)
}
