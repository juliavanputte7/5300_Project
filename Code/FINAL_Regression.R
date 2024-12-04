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
library(metR)
# Load in Ideal voltage function
source("Code/ideal_voltage_function.R")


# Read and Clean Data ###############
## Read in the data #########
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

## Group by every 30 seconds ###########
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

## Calculate quality metric #######
data = data %>% mutate(
  voltage_diff = (idealv - avg_bmsdcvoltage)^2
)

# Step 2: Create plots for each unique logdate
unique_dates <- unique(data$logdate)

# Plots ##########

## Plot Distance vs Voltage ########
data_dvsv <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
p = ggplot(data_dvsv, aes(x = avg_distance_km, y = avg_bmsdcvoltage)) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "30 Second Average Voltage Over Distance",
    x = "Distance (km)",
    y = "Average BMS DC Voltage",
    color = "Race Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

print(p)

ggsave( dpi = 300,
        filename = paste0("Code/General Plots/Distance vs Voltage.png"),
        plot = p,
        width = 7, height = 5
)

remove(data_dvsv)

## Plot Distance vs Current ########
data_dvsc <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
p = ggplot(data_dvsc, aes(x = avg_distance_km, y = avg_current)) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "30 Second Average BMS DC Current Over Distance",
    x = "Distance (km)",
    y = "BMS DC Current (A)",
    color = "Log Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

print(p)

ggsave(
  dpi = 300,
  filename = paste0("Code/General Plots/Distance vs Current.png"),
  plot = p,
  width = 7, height = 5
)

remove(data_dvsc)

## Plot SOC Voltage Curves ########
data_dvsc <- data %>%
  filter(vehicle == "ARG24") %>% 
  group_by(logdate) %>%
  mutate(
    avg_distance_km = avg_distance / 1000
  )

# Plot
p = ggplot(data_dvsc, aes(x = avg_soc, y = avg_bmsdcvoltage)) +
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

print(p)

ggsave(dpi = 300, width=7,height=5,
  filename = paste0("Code/General Plots/SOC Voltage Curves.png"),
  plot = p
)

remove(data_dvsc)


## Plot Ideal Voltage Curve ####
# Create the plot
p = ggplot(data, aes(x = distance_km, y = idealv)) +
  # Plot the ideal SOC curve
  geom_line(color = "blue", size = 1.5, linetype = "solid") +
  # Adding label for the ideal SOC curve using annotate()
  annotate(
    "text", 
    x = 18.75,  # Adjust x position
    y = 326.5, # Adjust y position
    label = "Ideal Voltage", 
    color = "blue", 
    size = 5, 
    hjust = 0,
    angle=303
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
    title = "Ideal vs Actual Voltage for 8 Races",
    color = "Race Date"
  ) +
  # Legend title
  scale_color_discrete(name = "Race Date") +
  # Theme
  theme_minimal()

print(p)

ggsave(
  dpi = 300,
  width = 7,
  height = 5,
  filename = paste0("Code/General Plots/Ideal Voltage.png"),
  plot = p
)

## Plot Voltage Difference Curve ####
# Create the plot
p = ggplot(data, aes(x = distance_km, y = voltage_diff)) +
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

print(p)

ggsave(
  dpi = 300,
  width = 7,
  height = 5,
  filename = paste0("Code/General Plots/Voltage Difference.png"),
  plot = p
)

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
corr_data = select(data, c(avg_current, time,inv_commandtq,groundspeed,packdcl,packccl,gforcelat,gforcelong, bms_dccurrent))
matrix = cor(corr_data)

# Rename for better visual
new_names <- c("30 sec Avg Current", "Time", "Torque", "Speed", "DCL", "CCL", "Lateral Acceleration", "Longitudinal Acceleration", "Current")
rownames(matrix) <- new_names
colnames(matrix) <- new_names

p = corrplot(matrix, type = "upper", tl.col = "black", tl.srt = 45,diag=FALSE, order = "alphabet") 


## inv_commandtq #######
# It looks like inv_commandtq and bms_dccurrent are highly correlated.
# Let's investigate

# Calculate correlation and p-value
correlation_test <- cor.test(data$inv_commandtq, data$bms_dccurrent)
correlation <- round(correlation_test$estimate, 2)
p_value <- round(correlation_test$p.value, 4)

# Create the plot
p = ggplot(data = data, aes(x = inv_commandtq, y = bms_dccurrent)) +
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

print(p)
ggsave(
  filename = paste0("Code/General Plots/Torque v Current.png"),
  plot = p
)


# We will just use inv_command_tq since these are so highly correlated

# Linear Regression ######

## Complex #######

### M0 #########
# only variables no interactions or poly
m0 = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable is average voltage. then in RSM we will minimize the error.
      distance + 
      packdcl +
      packccl + 
      gforcelong + 
      gforcelat +
      avg_current +
      inv_commandtq
  )
glance(m0)
print(tidy(m0), n=nrow(tidy(m0)))

### M1 #####
# adding some polynomial terms
# only add them if they increase r-sq by more than .005
m1 = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable
      poly(distance,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      gforcelong + # polynomial did not increase r-sq here
      gforcelat + # polynomial did not increase r-sq here
      poly(avg_current,3) + 
      groundspeed + # polynomial did not increase r-sq here
      inv_commandtq  # polynomial did not increase r-sq here
  )
glance(m1)
print(tidy(m1), n=nrow(tidy(m1)))

### M3 ####
# model with interactions and polynomials
m3 = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable
      # Polynomial terms
      poly(distance,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      gforcelong + # polynomial did not increase r-sq here
      gforcelat + # polynomial did not increase r-sq here
      poly(avg_current,3) + 
      groundspeed + # polynomial did not increase r-sq here
      inv_commandtq +  # polynomial did not increase r-sq here
      # interaction terms from m2
      I(distance * packdcl) +
      I(distance * packccl) +
      I(distance * gforcelong) +
      I(distance * gforcelat) +
      I(distance * avg_current) +
      I(distance * inv_commandtq) +
      I(packdcl * packccl) +
      I(packdcl * gforcelong) +
      I(packdcl * gforcelat) +
      I(packdcl * avg_current) +
      I(packdcl * inv_commandtq) +
      I(packccl * gforcelong) +
      I(packccl * gforcelat) +
      I(packccl * avg_current) +
      I(packccl * inv_commandtq) +
      I(gforcelong * gforcelat) +
      I(gforcelong * avg_current) +
      I(gforcelong * inv_commandtq) +
      I(gforcelat * avg_current) +
      I(gforcelat * inv_commandtq) +
      I(avg_current * inv_commandtq)
  )
glance(m3)
print(tidy(m3), n=nrow(tidy(m3)))

## Simple ##########
# We will not include gforce, torque, or speed in this model. this is because thewse cannot be tuned by cornell FSAE. they are fixed controls
# we wim for a more parsimonious model where we use ad few predictors as possible and still get a high r-sq
### M0 Simple #############
# only variables no interactions or poly
m0_s = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable is average voltage. then in RSM we will minimize the error.
      distance + 
      packdcl +
      packccl + 
      avg_current 
  )
glance(m0_s)
print(tidy(m0_s), n=nrow(tidy(m0_s)))

### M1 Simple #####
# adding some polynomial terms
# only add them if they increase r-sq by more than .005
m1_s = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable
      poly(distance,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      poly(avg_current,3) 
  )
glance(m1_s)
print(tidy(m1_s), n=nrow(tidy(m1_s)))


### M3 Simple ####
# model with interactions and polynomials
m3_s = data %>%
  lm(
    formula = 
      sqrt(voltage_diff) ~ # Response variable
      # Polynomial terms
      poly(distance,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      poly(packdcl,4) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      poly(packccl,3) + # Tried out a few polynomial terms for packccl, this caused the highest r-sq
      poly(avg_current,3) + 
      # interaction terms from m2
      I(distance * packdcl) +
      I(distance * packccl) +
      I(distance * avg_current) +
      I(packdcl * packccl) +
      I(packdcl * avg_current) +
      I(packccl * avg_current) 
  )
glance(m3_s)
print(tidy(m3_s), n=nrow(tidy(m3_s)))

# Compare the models
texreg::htmlreg(
  l = list(m0,m1,m3, m0_s,m1_s,m3_s),
  file = "Code/all_models.html"
)

browseURL("Code/all_models.html")

# Regression Plots #########

regression_data = data %>% 
  select(avg_current, distance,packdcl,packccl,voltage_diff)

# List of variables to plot
variables <- colnames(regression_data)[colnames(regression_data) != "voltage_diff"]   # Exclude 'time'

variable_names <- list(
  avg_current = "Average Current (A)",
  distance = "Distance (m)",
  packdcl = "Discharge Current Limit",
  packccl = "Charge Current Limit"
)

# Loop through variables and create plots
for (var in variables) {
  # Assign a variable-friendly name
  var_name <- variable_names[[var]]  # Get the name from the list
  
  # Create the plot
  p <- ggplot(regression_data, aes(x = regression_data[[var]], y = voltage_diff)) +
    geom_point(color = "blue", alpha = 0.5, size = 0.5) +
    labs(
      x = var_name,
      y = "Squared Error from Ideal Voltage",
      title = paste(var_name, "vs Error from Ideal Voltage")
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = paste0("Code/Regression Plots/Ideal_Voltage_Diff_vs_", gsub(" ", "_", var), ".png"),
    plot = p,
    dpi = 300,
    width = 7,
    height = 5
  )
}

# Make Predictions ###########

regression_data = data %>% select(avg_current, packdcl,packccl,voltage_diff,logdate,avg_distance)

prediction_data = data %>% select(distance,avg_current, packdcl,packccl)
predictions <- predict(m3_s, newdata = prediction_data)

regression_data = regression_data %>% mutate(predicted_vdiff = predictions^2)

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
    title = "Squared Error from Ideal Voltage vs Model Prediction (Grey) Over Distance",
    x = "Distance (km)",
    y = "Voltage Difference from Ideal (Squared)",
    color = "Race Date"
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

ggsave(
  filename = paste0("Code/General Plots/Predictions.png"),
  plot = p,
  dpi = 300,
  width = 7,
  height = 5
)

remove(data_dvsv)


# RSM - With CIs ###########

## DCL ########
grid = expand_grid(
  distance = seq(from=0, to = max(data$distance), by=10),
  packdcl = seq(from = 15, to = max(data$packdcl), by = 1),
) %>%
  mutate(packccl = 25, #This is the mode
         avg_current = median(data$avg_current))

# Make prediction with confidence interval
prediction = predict(m3_s, newdata = grid, interval = "confidence", level = 0.95)

# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = prediction[, "fit"]^2,
    lower_ci = prediction[, "lwr"]^2,
    upper_ci = prediction[, "upr"]^2
  )
remove(prediction)

library(ggplot2)
library(viridis)

# Add a column to the dataset to define line type for each contour
grid <- grid %>%
  mutate(line_type = case_when(
    between(yhat, 0.009, 0.011) ~ "Smallest Error",
    TRUE ~ "95% CI"
  ))

p_DCL <- ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance / 1000, y = packdcl, fill = yhat)) +
  
  # Main contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packdcl, z = yhat),
    binwidth = 1000,
    color = "white",
    size = 0.5
  ) +
  
  # Lower CI contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packdcl, z = lower_ci, linetype = "95% CI"),
    color = "grey",
    size = 0.5
  ) +
  
  # Smallest error line
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packdcl, z = yhat, linetype = "Smallest Error"),
    breaks = c(.01),
    color = "green2",
    size = 1
  ) +
  
  # Upper CI contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packdcl, z = upper_ci, linetype = "95% CI"),
    color = "grey",
    size = 0.5
  ) +
  
  # Add text labels for contours
  geom_text_contour(
    data = grid, 
    mapping = aes(x = distance / 1000, y = packdcl, z = yhat), 
    skip = 1, 
    stroke.color = "white", 
    stroke = 0.2, 
    label.placer = label_placer_n(1)
  ) +
  
  # Color fill
  viridis::scale_fill_viridis(option = "plasma") +
  
  # Manually add lines to legend
  scale_linetype_manual(
    name = " ",
    values = c("Smallest Error" = "solid", "95% CI" = "dashed")
  ) +
  scale_color_manual(
    name = " ",
    values = c("Smallest Error" = "green2", "95% CI" = "grey")
  ) +
  
  # Labels and themes
  labs(
    title = "RSM for DCL",
    subtitle = "Predicted Voltage Error by Discharge Current Limit",
    x = "Distance (km)",
    y = "Discharge Current Limit",
    fill = "Squared Error \n from Ideal \n Voltage"
  ) +
  theme_minimal()

print(p_DCL)


# Save the plot
ggsave(
  filename = paste0("Code/RSM Plots/Ideal_Voltage_Diff_vs_DCL_with_CIs.png"),
  plot = p_DCL,
  dpi = 300,
  width = 7,
  height = 5
)

bestdcl = grid %>%
  filter(yhat == min(yhat))
bestdcl

## CCL ########
grid = expand_grid(
  distance = seq(from=0, to = max(data$distance), by=10),
  packccl = seq(from = min(data$packccl), to = max(data$packccl), by = .25),
) %>%
  mutate(packdcl = 30, #This is the mode
         avg_current = median(data$avg_current))

# Make prediction with confidence interval
prediction = predict(m3_s, newdata = grid, interval = "confidence", level = 0.95)

# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = prediction[, "fit"]^2,
    lower_ci = prediction[, "lwr"]^2,
    upper_ci = prediction[, "upr"]^2
  )

p_CCL <- ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance / 1000, y = packccl, fill = yhat)) +
  
  # Main contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packccl, z = yhat),
    color = "white", linetype = "solid",
    binwidth = 500,
    size = 0.5
  ) +
  
  # Lower CI contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packccl, z = lower_ci, color = "95% CI", linetype = "95% CI"),
    binwidth = 500,
    size = 0.5
  ) +
  
  # Upper CI contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packccl, z = upper_ci, color = "95% CI", linetype = "95% CI"),
    binwidth = 500,
    size = 0.5
  ) +
  
  # Smallest Error line
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packccl, z = yhat, color = "Smallest Error", linetype = "Smallest Error"),
    breaks = c(.01),
    size = 1
  ) +
  
  # Add text labels for contours
  geom_text_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = packccl, z = yhat),
    skip = 0,
    stroke.color = "white",
    stroke = 0.2,
    label.placer = label_placer_n(1)
  ) +
  
  # Color fill
  viridis::scale_fill_viridis(option = "plasma") +
  
  # Manually add lines to legend
  scale_linetype_manual(
    name = " ",
    values = c("95% CI" = "dashed", "Smallest Error" = "solid")
  ) +
  scale_color_manual(
    name = " ",
    values = c("95% CI" = "grey", "Smallest Error" = "green2")
  ) +
  
  # Labels and themes
  labs(
    title = "RSM for CCL",
    subtitle = "Predicted Voltage Error by Charge Current Limit",
    x = "Distance (km)",
    y = "Charge Current Limit",
    fill = "Squared Error \n from Ideal \n Voltage"
  ) +
  theme_minimal()

print(p_CCL)


# Save the plot
ggsave(
  filename = paste0("Code/RSM Plots/Ideal_Voltage_Diff_vs_CCL_WithCIs.png"),
  plot = p_CCL,
  dpi = 300,
  width = 7,
  height = 5
)

bestccl = grid %>%
  filter(yhat == min(yhat))
bestccl

## Average Current ########
grid = expand_grid(
  distance = seq(from=0, to = max(data$distance), by=10),
  avg_current = seq(from = min(data$avg_current), to = max(data$avg_current), by = 1),
) %>%
  mutate(packdcl = 30, #This is the mode
         packccl = 25)

# Make prediction with confidence interval
prediction = predict(m3_s, newdata = grid, interval = "confidence", level = 0.95)

# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = prediction[, "fit"]^2,
    lower_ci = prediction[, "lwr"]^2,
    upper_ci = prediction[, "upr"]^2
  )

# Add contour tolerance
p <- ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance / 1000, y = avg_current, fill = yhat)) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
    binwidth = 1000,
    color = "white",
    size = 0.8
  ) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = lower_ci),
    binwidth = 1000,
    color = "grey",
    size = 0.5,
    linetype = "dashed"
  ) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = upper_ci),
    binwidth = 1000,
    color = "grey",
    size = 0.5,
    linetype = "dashed"
  ) +
  geom_text_contour(data = grid, mapping = aes(x = distance/1000, y= avg_current, z = yhat), 
                    skip = 0, stroke.color = "white", stroke = 0.2, label.placer = label_placer_n(1))+
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
    breaks = c(.01),
    color = "white",
    size = 1
  ) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Voltage Error by Average Current - With 95% Confidence Intervals",
    x = "Distance (km)",
    y = "Average Current",
    fill = "Squared Error from Ideal Voltage"
  ) +
  theme_minimal()

print(p)

# Save the plot
ggsave(
  filename = paste0("Code/RSM Plots/Ideal_Voltage_Diff_vs_Average Current_WithCIs.png"),
  plot = p
)

bestcurrent = grid %>%
  filter(yhat == min(yhat))

# Parameter Optimization############

# bestcurrent, bestdcl, bestccl is all parameter combinations where error is minimized
# We will find the parameters to get us the closest to error of 0!

bestcurrent = bestcurrent$avg_current
bestcurrent
# 20.19853
# But, many currents seem to get an error close to 0

bestdcl = bestdcl$packdcl
bestdcl
# 33

bestccl = bestccl$packccl
bestccl
# 29

## RSM with optimal DCL CCL
## Average Current ########
grid = expand_grid(
  distance = seq(from=0, to = max(data$distance), by=10),
  avg_current = seq(from = min(data$avg_current), to = max(data$avg_current), by = 1),
) %>%
  mutate(packdcl = 33, #This is the mode
         packccl = 29)

# Make prediction with confidence interval
prediction = predict(m3_s, newdata = grid, interval = "confidence", level = 0.95)

# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = prediction[, "fit"]^2,
    lower_ci = prediction[, "lwr"]^2,
    upper_ci = prediction[, "upr"]^2
  )

# Add contour tolerance
p <- ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance / 1000, y = avg_current, fill = yhat)) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
    binwidth = 1000,
    color = "white",
    size = 0.8
  ) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = lower_ci),
    binwidth = 1000,
    color = "grey",
    size = 0.5,
    linetype = "dashed"
  ) +
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = upper_ci),
    binwidth = 1000,
    color = "grey",
    size = 0.5,
    linetype = "dashed"
  ) +
  geom_text_contour(data = grid, mapping = aes(x = distance/1000, y= avg_current, z = yhat), 
                    skip = 0, stroke.color = "white", stroke = 0.2, label.placer = label_placer_n(1))+
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
    breaks = c(.01),
    color = "white",
    size = 1
  ) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Voltage Error by Average Current - With 95% Confidence Intervals",
    x = "Distance (km)",
    y = "Average Current",
    fill = "Squared Error from Ideal Voltage"
  ) +
  theme_minimal()

print(p)

# Save the plot
ggsave(
  filename = paste0("Code/RSM Plots/RSM With Optimal DCL CCL.png"),
  plot = p
)

# Get best currents at each distance
best_current_5000 = grid %>% filter(distance==5000) %>% filter(yhat == min(yhat))
best_current_10000 = grid %>% filter(distance==10000) %>% filter(yhat == min(yhat))
best_current_15000 = grid %>% filter(distance==15000) %>% filter(yhat == min(yhat))
opt_currents = c(best_current_5000$avg_current,best_current_10000$avg_current,best_current_15000$avg_current)

## Optimal Prediction ######

grid_pred = expand_grid(
  distance = seq(from=5000, to = 15000, by=5000)
) %>%
  mutate(packdcl = 33, #This is the optimal
         packccl = 29, # This is the optimal
         avg_current = opt_currents) # Current seems to be the best at the maximum level as seeen in the RSM

# Make prediction with confidence interval
prediction = predict(m3_s, newdata = grid, interval = "confidence", level = 0.95)

# Get the grid with confidence interval
grid_pred = grid_pred %>% 
  mutate(
    yhat = prediction[, "fit"]^2,
    lower_ci = prediction[, "lwr"]^2,
    upper_ci = prediction[, "upr"]^2
  )
grid_pred

# A tibble: 3 × 7
# distance packdcl packccl avg_current    yhat lower_ci upper_ci
# <dbl>   <dbl>   <dbl>       <dbl>   <dbl>    <dbl>    <dbl>
# 1     5000      33      29        13.2 0.00549  0.00920   0.0596
# 2    10000      33      29        22.2 0.243    0.457     0.0961
# 3    15000      33      29        38.2 0.111    0.0240    0.675 

## Optimal Current varies by distance #########
# Calculate optimal currents for a range of distances
distances <- seq(0, 21999, by = 1000)  # Example distances
optimal_currents <- c()

for (d in distances) {
  best_current <- grid %>% 
    filter(distance == d) %>% 
    filter(yhat == min(yhat)) %>% 
    pull(avg_current)
  optimal_currents <- c(optimal_currents, best_current)
}

# Create a data frame with distances and optimal currents
optimal_data <- tibble(
  distance = distances,
  optimal_current = optimal_currents
)

# Plot distance vs optimal current
ggplot(optimal_data, aes(x = distance, y = optimal_current)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Distance vs Optimal Current",
    x = "Distance (m)",
    y = "Optimal Current (A)"
  ) +
  theme_minimal()



