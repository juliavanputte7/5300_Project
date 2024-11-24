# Cool Kids Regression Model and RSM

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("corrplot")
#install.packages("broom")

# Load Packages
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(broom)
library(tidyr)
library(tidyverse)
library(viridis)
library(texreg)
library(corrplot)

# Load in Ideal voltage function
source("Code/ideal_voltage_function.R")

# Read in the data #########
data = read_csv("Data/COMBINED_With_Ground_Speed_CLEANED_1Decimal (1).csv") %>% 
  setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", "")) 

# Remove NAs
data <- data %>% filter(!is.na(distance))

# Remove rows where distance is greater than 22km
data = data %>% filter(distance <= 22000)

# Remove columns we do not need (leftover from previous model where we use SOC)
data <- data %>% select(-idealsoc, -soc_delta)

# Convert gforce to absolute and get ideal voltage
data = data %>% mutate(gforcelat = abs(gforcelat),
                       gforcelong = abs(gforcelong),
                       distance_km = distance/1000,
                       idealv = ideal_voltage(distance_km))

# Group by every 30 seconds ###########
# Step 1: Create a new column for the 30-second time group
data <- data %>%
  group_by(logdate) %>% 
  mutate(time_group = floor(second / 30) * 30) %>% 
  ungroup()

# Step 2: Calculate the average bmsdcvoltage and distance and current for each logdate and time group
data <- data %>%
  group_by(logdate, time_group) %>%
  mutate(avg_bmsdcvoltage = mean(bmsdcvoltage, na.rm = TRUE),
         avg_distance = mean(distance), na.rm=TRUE,
         avg_current = mean(bms_dccurrent), na.rm=TRUE,
         avg_soc = mean(packsoc), na.rm=TRUE) %>%
  ungroup()

# Calculate quality metric #######
data = data %>% mutate(
  voltage_diff = (idealv - avg_bmsdcvoltage)^2
)

# Step 2: Create plots for each unique logdate
unique_dates <- unique(data$logdate)

# Plot Distance vs Voltage ########
data_dvsv <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
ggplot(data_dvsv, aes(x = avg_distance_km, y = avg_bmsdcvoltage)) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "30 Second Average Voltage Over Distance",
    x = "Distance (km)",
    y = "Average BMS DC Voltage",
    color = "Race Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

remove(data_dvsv)

# Plot Distance vs Current ########
data_dvsc <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
ggplot(data_dvsc, aes(x = avg_distance_km, y = avg_current)) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "30 Second Average BMS DC Current Over Distance",
    x = "Distance (km)",
    y = "BMS DC Current (A)",
    color = "Log Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

remove(data_dvsc)

# Plot SOC Voltage Curves ########
data_dvsc <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
ggplot(data_dvsc, aes(x = avg_soc, y = avg_bmsdcvoltage)) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "SOC Curve for Our Data",
    x = "State of Charge (%)",
    y = "Voltage (V) - 30 sec average",
    color = "Log Date"
  ) +
  theme_minimal() +
  scale_x_reverse() + 
  facet_wrap(~logdate, ncol = 2)

remove(data_dvsc)


# Plot Ideal Voltage Curve ####
# Create the plot
ggplot(data, aes(x = distance_km, y = idealv)) +
  # Plot the ideal SOC curve
  geom_line(color = "blue", size = 1.5, linetype = "solid") +
  # Adding label for the ideal SOC curve using annotate()
  annotate(
    "text", 
    x = max(data$distance_km) * 0.85,  # Adjust x position
    y = min(data$avg_bmsdcvoltage) * 1.15, # Adjust y position
    label = "Ideal Voltage Curve", 
    color = "blue", 
    size = 5, 
    hjust = 0,
    angle=305
  ) +
  # Plot SOC curves for each unique date
  geom_line(
    data = data %>% group_by(logdate), 
    aes(x = avg_distance/1000, y = avg_bmsdcvoltage, color = as.factor(logdate)),
    size = 0.8
  ) +
  # Labels and legend
  labs(
    x = "Distance (km)",
    y = "Voltage (V)",
    title = "Voltage Curves for 8 Races vs Ideal",
    color = "Race Date"
  ) +
  # Legend title
  scale_color_discrete(name = "Race Date") +
  # Theme
  theme_minimal()

# Plot Voltage Difference Curve ####
# Create the plot
ggplot(data, aes(x = distance_km, y = voltage_diff)) +
  # Plot SOC curves for each unique date
  geom_line(
    data = data %>% group_by(logdate), 
    aes(x = distance_km, y = voltage_diff, color = as.factor(logdate)),
    size = 0.8
  ) +
  # Labels and legend
  labs(
    x = "Distance (km)",
    y = "Voltage Difference from Ideal Squared (V)",
    title = "Voltage Difference (Quality Metric) Curves for 8 Races",
    color = "Race Date"
  ) +
  # Legend title
  scale_color_discrete(name = "Race Date") +
  # Theme
  theme_minimal()


# Pick possible variables to include in the regression#########
# Possible Variables
columns = names(data)
print(columns)

# Possible variables to include in regression
# logdate time gforcelat gforcelong packdcl batttemphi batttemplo inv_commandtq
# motorspeed bms_dccurrent packccl distance groundspeed vehicle

# distance = speed * time
# We will just consider distance and speed, since these are most important for racing

# motorspeed and groundspeed can be converted between one another, we will jsut consider ground speed since it is more interpretable
# ground speed is in meters per second

# logdate should maybe not be a predictor because we want to be able to find the optimal SOC discharge
# based on car settings and not just based on the date of the test

# Correlation Plot ########
# We want to avoid collinearity so we will plot the covariance matrix
# Select numeric data and make sure not correlated
corr_data = select(data, c(avg_current, time,packdcl,packccl))
matrix = cor(corr_data)

# Rename for better visual
new_names <- c("30 sec Avg Current", "Time", "DCL", "CCL")
rownames(matrix) <- new_names
colnames(matrix) <- new_names

corrplot(matrix, type = "upper", tl.col = "black", tl.srt = 45,diag=FALSE, order = "alphabet") 

## inv_commandtq #######
# It looks like inv_commandtq and bms_dccurrent are highly correlated.
# Let's investigate

# Calculate correlation and p-value
correlation_test <- cor.test(data$inv_commandtq, data$bms_dccurrent)
correlation <- round(correlation_test$estimate, 2)
p_value <- round(correlation_test$p.value, 4)

# Create the plot
ggplot(data = data, aes(x = inv_commandtq, y = bms_dccurrent)) +
  geom_point(size = .5, alpha=.5) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of Torque vs. Current",
    x = "Inv Command TQ",
    y = "BMS DC Current"
  ) +
  annotate(
    "text", 
    x = Inf, y = Inf, 
    label = paste("Correlation:", correlation, "\nP-value: <.001"),
    hjust = 1.1, vjust = 1.5, 
    size = 4,
    color = "blue"
  )

# We will just use inv_command_tq since these are so highly correlated

# Linear Regression ######


## M0 #########
# only variables no interactions or poly
m0 = data %>%
  lm(
    formula = 
      voltage_diff ~ # Response variable is average voltage. then in RSM we will minimize the error.
      time + 
      packdcl +
      packccl + 
      avg_current 
  )
glance(m0)
print(tidy(m0), n=nrow(tidy(m0)))

## M1 #####
# adding some polynomial terms
# only add them if they increase r-sq by more than .005
m1 = data %>%
  lm(
    formula = 
      voltage_diff ~ # Response variable
      poly(time,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      poly(avg_current,3) 
  )
glance(m1)
print(tidy(m1), n=nrow(tidy(m1)))

## M2 #######
# Model with interaction terms (no polynomial)
m2 = data %>%
  lm(
    formula = 
      voltage_diff ~ # Response variable
      time + 
      packdcl +
      packccl + 
      avg_current +
      I(time * packdcl) +
      I(time * packccl) +
      I(time * avg_current) +
      I(packdcl * packccl) +
      I(packdcl * avg_current) +
      I(packccl * avg_current) 
  )
glance(m2)
print(tidy(m2), n=nrow(tidy(m2)))

## M3 ####
# model with interactions and polynomials
m3 = data %>%
  lm(
    formula = 
      voltage_diff ~ # Response variable
      # Polynomial terms
      poly(time,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      poly(avg_current,3) + 
      # interaction terms from m2
      I(time * packdcl) +
      I(time * packccl) +
      I(time * avg_current) +
      I(packdcl * packccl) +
      I(packdcl * avg_current) +
      I(packccl * avg_current) 
  )
glance(m3)
print(tidy(m3), n=nrow(tidy(m3)))

# Compare the models
texreg::htmlreg(
  l = list(m0,m1,m2,m3),
  file = "model.html"
)

browseURL("model.html")

# Regression Plots #########

regression_data = data %>% select(avg_current, time,packdcl,packccl,voltage_diff)

# List of variables to plot
variables <- colnames(regression_data)[colnames(regression_data) != "voltage_diff"]   # Exclude 'time'

# Loop through variables and create plots
for (var in variables) {
  p <- ggplot(regression_data, aes(x = regression_data[[var]], y = voltage_diff)) +
    geom_point(color = "blue", alpha = 0.5, size = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
    labs(
      x = var,
      y = "Voltage Delta (V)",
      title = paste(var, " vs Voltage Delta with Regression Line")
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0("Code/Regression Plots/Voltage_vs_", var, ".png"), plot = p)  # To save as PNG
}


# Make Predictions ###########

regression_data = data %>% select(avg_current, time,packdcl,packccl,voltage_diff,logdate,avg_distance)

prediction_data = data %>% select(avg_current, time,packdcl,packccl)
predictions <- predict(m3, newdata = prediction_data)

regression_data = regression_data %>% mutate(predicted_vdiff = predictions)

# Plot Distance vs Voltage vs Prediction ########
data_dvsv <- regression_data %>%
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
ggplot(data_dvsv, aes(x = avg_distance_km, y = voltage_diff)) +
  geom_line(data = data_dvsv, aes(x=avg_distance_km, y=predicted_vdiff),color='darkgrey') +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  # Plot the ideal SOC curve
  #geom_line(data = data, aes(x = distance_km, y = idealv), color = "Ideal", size = 1.5, linetype = "solid") +
  labs(
    title = "Voltage Difference from Ideal vs Prediction (Grey)  Over Distance",
    x = "Distance (km)",
    y = "Voltage Difference from Ideal (Squared)",
    color = "Race Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

remove(data_dvsv)




# RSM #########


library(tidyverse)
library(viridis)

# Helper function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Define the variables
variables <- c("avg_current", "time",
               "packdcl", "packccl")

# Generate all combinations of variables (pairwise)
combinations <- combn(variables, 2, simplify = FALSE)

# Loop over combinations
for (pair in combinations) {
  var1 <- pair[1]
  var2 <- pair[2]
  
  # Create a grid for the current pair of variables
  grid <- expand_grid(
    !!sym(var1) := seq(from = min(data[[var1]], na.rm = TRUE), 
                       to = max(data[[var1]], na.rm = TRUE), 
                       length.out = 500),
    !!sym(var2) := seq(from = min(data[[var2]], na.rm = TRUE), 
                       to = max(data[[var2]], na.rm = TRUE), 
                       length.out = 500)
  )
  
  # Set other variables to their default values
  other_vars <- setdiff(variables, c(var1, var2))
  for (var in other_vars) {
    if (is.numeric(data[[var]])) {
      grid[[var]] <- mean(data[[var]], na.rm = TRUE)
    } else {
      grid[[var]] <- getmode(data[[var]])
    }
  }
  
  # Predict values
  grid <- grid %>%
    mutate(yhat = pmax(0, predict(m3, newdata = .)))
  
  # Create a heatmap plot
  plot <- ggplot() +
    geom_tile(data = grid, mapping = aes(x = .data[[var1]], y = .data[[var2]], fill = yhat)) +
    viridis::scale_fill_viridis(option = "plasma") +
    labs(
      title = paste("Predicted Outcome for", var1, "and", var2),
      x = var1,
      y = var2,
      fill = "Predicted Voltage Delta"
    ) +
    theme_minimal()
  
  # Print or save the plot
  print(plot)
  ggsave(filename = paste0("Code/SIMPLE/RSM Plots/RSM_", var1, "_vs_", var2, ".png"), plot = plot)
}





getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# avg_current, time,inv_commandtq,groundspeed,packdcl,packccl,gforcelat,gforcelong

## Average Current ##########
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  avg_current = seq(from = min(data$avg_current), to = max(data$avg_current), by = 1),
) %>%
  mutate(gforcelat = mean(data$gforcelat),
         gforcelong = mean(data$gforcelong),
         packccl = getmode(data$packccl),
         packdcl = getmode(data$packdcl),
         groundspeed = mean(data$groundspeed),
         inv_commandtq = mean(data$inv_commandtq)) %>%
  mutate(yhat = predict(m4, newdata = tibble(.) )   )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = avg_current, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by 30 Second Average Current",
    x = "Time(s)",
    y = "Average Current",
    fill = "Predicted Voltage Delta"
  ) +
  theme_minimal()

grid %>%
  filter(yhat == min(yhat))

# inv_commandtq,groundspeed,packdcl,packccl,gforcelat,gforcelong

## Torque ##########
grid = expand_grid(
  avg_current = seq(from= min(data$avg_current), to = max(data$avg_current), by=1),
  inv_commandtq = seq(from = min(data$inv_commandtq), to = max(data$inv_commandtq), by = 1),
) %>%
  mutate(gforcelat = mean(data$gforcelat),
         gforcelong = mean(data$gforcelong),
         packccl = getmode(data$packccl),
         packdcl = getmode(data$packdcl),
         groundspeed = mean(data$groundspeed),
         time = 1000) %>%
  mutate(yhat = predict(m4, newdata = tibble(.) )   )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = avg_current, y = inv_commandtq, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Torque",
    x = "Time(s)",
    y = "Torque",
    fill = "Predicted Voltage Delta"
  ) +
  theme_minimal()

# 
# # avg_current, time,inv_commandtq,groundspeed,packdcl,packccl,gforcelat,gforcelong
# 
# grid = expand_grid(
#   time = c(500,1000,2000),
#   packdcl = seq(from = min(data$packdcl), to = 200, by = .1),
#   packccl = seq(from = min(data$packccl), to = 30, by = 1)
# ) %>%
#   mutate(gforcelat = 1,
#          gforcelong = 1,
#          avg_current = 25,
#          groundspeed = 15,
#          inv_commandtq = 50) %>%
#   mutate(yhat = predict(m4, newdata = tibble(.) ) )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = packdcl, y = packccl, fill = yhat)) +
#   facet_wrap(~time) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Pack DCL and Pack CCL",
#     subtitle = "for time = 500s, 1000s, 2000s",
#     x = "Pack DCL",
#     y = "Pack CCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# ## DCL #####
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   packdcl = seq(from = min(data$packdcl), to = max(data$packdcl), by = 1),
# ) %>%
#   mutate(gforcelat = 1,
#          gforcelong = 1,
#          packccl = 25,
#          avg_current = 30,
#          groundspeed = 15,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m4, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = packdcl, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Pack DCL",
#     x = "Time(s)",
#     y = "Pack DCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# 
# grid %>%
#   filter(yhat <= min(yhat)+min(yhat)*.01)
# 
# 
# 
# ## DCL Upper Confidence Interval #####
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=10),
#   packdcl = seq(from = min(data$packdcl), to = max(data$packdcl), by = 10),
# ) %>%
#   mutate(gforcelat = 1,
#          gforcelong = 1,
#          packccl = 25,
#          avg_current = 30,
#          groundspeed = 15,
#          inv_commandtq = 20) %>%
#   mutate(predict(m0, newdata = tibble(.), se.fit = TRUE) %>%
#            as_tibble() %>%
#            select(yhat = fit, se = se.fit)) %>%
#   mutate(upper = yhat + se * qnorm(0.975))
# # NOTE: SHOULD YOU USE PLOGIS() ?
# 
# # grid %>%
# #   mutate(id = 1:n()) %>%
# #   group_by(id) %>%
# #   reframe(ysim = rnorm(n = 1000, mean = yhat, sd = se) ) %>%
# #   ungroup() %>%
# #   group_by(id) %>%
# #   summarize()
# 
# 
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = packdcl, fill = upper)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Pack DCL",
#     x = "Time(s)",
#     y = "Pack DCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# 
# grid %>%
#   filter(yhat <= min(yhat)+min(yhat)*.01)
# 
# 
# # Want to minimize pack DCL, at least less than 50
# 
# ## CCL #####
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   packccl = seq(from = min(data$packccl), to = max(data$packccl), by = 1),
# ) %>%
#   mutate(gforcelat = 1,
#          gforcelong = 1,
#          packdcl = 25,
#          avg_current = 10,
#          groundspeed = 15,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = packccl, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Pack CCL",
#     x = "Time(s)",
#     y = "Pack CCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# 
# grid %>%
#   filter(yhat <= min(yhat)+min(yhat)*.01)
# 
# ### Avg Current #######
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   avg_current = seq(from = min(data$avg_current), to = max(data$avg_current), by = 1),
# ) %>%
#   mutate(gforcelat = 1,
#          gforcelong = 1,
#          packdcl = 30,
#          packccl = 30,
#          groundspeed = 15,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m2, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = avg_current, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Average Current",
#     x = "Time(s)",
#     y = "Average Current (30 second)",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# ## Gforcelong ########
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   gforcelong = seq(from = min(data$gforcelong), to = max(data$gforcelong), by = .1),
# ) %>%
#   mutate(batttemplo = 19,
#          packdcl = 30,
#          packccl = 30,
#          avg_current = 30,
#          groundspeed = 15,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = gforcelong, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by G Force",
#     x = "Time(s)",
#     y = "G Force Lat",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# 
# ## Ground Speed ########
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   groundspeed = seq(from = min(data$groundspeed), to = max(data$groundspeed), by = .5),
# ) %>%
#   mutate(batttemplo = 19,
#          packdcl = 30,
#          packccl = 30,
#          batttemphi = 50,
#          inv_commandtq = 20,
#          gforcelat = 0) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = groundspeed, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Ground Speed",
#     x = "Time(s)",
#     y = "Ground Speed",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# ## Torque ########
# grid = expand_grid(
#   time = seq(from=0, to = max(data$time), by=1),
#   inv_commandtq = seq(from = min(data$inv_commandtq), to = max(data$inv_commandtq), by = 1),
# ) %>%
#   mutate(batttemplo = 19,
#          packdcl = 30,
#          packccl = 30,
#          batttemphi = 50,
#          groundspeed = 10,
#          gforcelat = 0) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = inv_commandtq, fill = yhat)) +
#   viridis::scale_fill_viridis(option = "plasma") +
#   labs(
#     title = "Predicted Outcome by Torque",
#     x = "Time(s)",
#     y = "Torque",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# ## Summary Plot ########
# 
# # Pack CCL and Pack DCL
# grid = expand_grid(
#   packdcl = c(1,50,100,150,200),
#   time = seq(from=0, to = max(data$time), by=10),
#   packccl = seq(from=min(data$packccl), to= max(data$packccl), by=1)
# ) %>%
#   mutate(gforcelat = .1,
#          groundspeed = 10,
#          batttemphi = 50,
#          batttemplo = 19,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = packccl, fill = yhat)) + 
#   viridis::scale_fill_viridis(option = "plasma") +
#   facet_wrap(~packdcl) +
#   labs(
#     title = "Predicted Error by DCL and CCL, with Optimized Parameters",
#     subtitle = "For Pack DCL 1, 50, 100, 150, 200",
#     x = "Time",
#     y = "CCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
# 
# # Pack CCL and Pack DCL
# grid = expand_grid(
#   packccl = c(1,10,15,20,30),
#   time = seq(from=0, to = max(data$time), by=10),
#   packdcl = seq(from=min(data$packdcl), to= max(data$packdcl), by=1)
# ) %>%
#   mutate(gforcelat = .1,
#          groundspeed = 10,
#          batttemphi = 50,
#          batttemplo = 19,
#          inv_commandtq = 20) %>%
#   mutate(yhat = predict(m0, newdata = tibble(.) )   )
# 
# ggplot() +
#   geom_tile(data = grid, mapping = aes(x = time, y = packdcl, fill = yhat)) + 
#   viridis::scale_fill_viridis(option = "plasma") +
#   facet_wrap(~packccl) +
#   labs(
#     title = "Predicted Error by DCL and CCL, with Optimized Parameters",
#     subtitle = "For Pack CCL 1, 10, 15, 20, 30",
#     x = "Time",
#     y = "DCL",
#     fill = "Predicted Value (yhat)"
#   ) +
#   theme_minimal()
