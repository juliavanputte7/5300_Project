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
library(ggplot2)
library(viridis)
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
                       total_acceleration = gforcelat + gforcelong,
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
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 16), axis.title = element_text(size = 14) )


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
corr_data = select(data, c(avg_current, distance,inv_commandtq,groundspeed,packdcl,packccl,gforcelat,gforcelong, bms_dccurrent))
matrix = cor(corr_data)

# Rename for better visual
new_names <- c("30 sec Avg Current", "Distance", "Torque", "Speed", "DCL", "CCL", "Lateral Acceleration", "Longitudinal Acceleration", "Current")
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
  dpi = 300,
  width = 7,
  height = 5,
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
      groundspeed + 
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
  select(avg_current, distance,packdcl,packccl,voltage_diff, gforcelat, 
         gforcelong, batttemphi, batttemplo, inv_commandtq, groundspeed,total_acceleration )

# List of variables to plot
variables <- colnames(regression_data)[colnames(regression_data) != "voltage_diff"]   # Exclude 'time'

variable_names <- list(
  avg_current = "Average Current (A)",
  distance = "Distance (m)",
  packdcl = "Discharge Current Limit",
  packccl = "Charge Current Limit",
  gforcelat = "Lateral G Force",
  gforcelong = "Longitudinal G Force",
  total_acceleration ="Acceleration",
  batttemphi = "Battery Temperature High",
  batttemplo = "Battery Temperature Low",
  inv_commandtq = "Torque",
  groundspeed = "Ground Speed"
  
  
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

p = ggplot(data_dvsv, aes(x = avg_distance_km, y = voltage_diff)) +
  geom_line(data = data_dvsv, aes(x = avg_distance_km, y = predicted_vdiff, color = "Model Prediction"), size = 1) +
  geom_line(aes(color = as.factor(logdate)), size = 1) +
  labs(
    title = "Squared Error from Ideal Voltage vs Model Prediction Over Distance",
    x = "Distance (km)",
    y = "Voltage Difference from Ideal (Squared)",
    color = "Legend"
  ) +
  scale_color_manual(
    values = c("Model Prediction" = "black", 
               setNames(scales::hue_pal()(length(unique(data_dvsv$logdate))), unique(data_dvsv$logdate))),
    breaks = c("Model Prediction", as.character(unique(data_dvsv$logdate)))
  ) +
  theme_minimal() +
  facet_wrap(~logdate, ncol = 2)

p

scales::h



ggsave(
  filename = paste0("Code/General Plots/Predictions.png"),
  plot = p,
  dpi = 300,
  width = 7,
  height = 5
)

remove(data_dvsv)



# Monte Carlo Sampling Procedure

#' #' @name get_se
#' #' @description 
#' #' Approximate the standard error by using the mean fitted  value as our benchmark point
#' get_se = function(model, prob = 0.5){
#'   # Testing data
#'   # model = m3_s
#'   
#'   # model$fitted.values
#'   # predict(model)
#'   
#'   
#'   yhat_sqrt = quantile(model$fitted.values, prob = prob)
#'   sigma_sqrt = broom::glance(model)$sigma
#'   
#'   ysim = rnorm(n = 1000, mean = yhat_sqrt, sd = sigma_sqrt)^2
#'   
#'   sigma = sd(ysim)
#'   
#'   return(sigma)
#' }
#' 
#' get_se(model = m3_s, prob = 0.25)
#' get_se(model = m3_s, prob = 0.5)
#' get_se(model = m3_s, prob = 0.75)

# RSM - With CIs ###########

## DCL ########
grid = expand_grid(
  distance = seq(from=0, to = max(data$distance), by=300),
  packdcl = seq(from = 15, to = max(data$packdcl), by = 1),
) %>%
  mutate(packccl = 25, #This is the mode
         avg_current = median(data$avg_current)) %>%
  mutate(id = 1:n())

# tibble(rep = 1:1000) %>%
#   group_by(rep) %>%
#   reframe(grid) 

grid = grid %>%
  mutate(yhat = predict(m3_s, newdata = .),
         se = glance(m3_s)$sigma)
  
# Monte carlo sampling
grid = grid %>% 
  group_by(id) %>%
  reframe(
    ysim = rnorm(n = 10000, mean = yhat, sd = se),
    ysim = ysim^2
  ) %>%
  group_by(id) %>%
  summarize(
    upper = quantile(ysim, probs = 0.95)) %>%
  left_join(by = "id", y = grid)


# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = grid$yhat^2,
  )


# Add a column to the dataset to define line type for each contour
grid <- grid %>%
  mutate(line_type = case_when(
    between(yhat, 0, 0.011) ~ "Smallest Error",
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
    mapping = aes(x = distance / 1000, y = packdcl, z = upper, linetype = "Upper 95% CI"),
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
    values = c("Smallest Error" = "solid", "Upper 95% CI" = "dashed")
  ) +
  scale_color_manual(
    name = " ",
    values = c("Smallest Error" = "green2", "Upper 95% CI" = "grey")
  ) +
  
  # Labels and themes
  labs(
    title = "RSM for DCL",
    subtitle = "Predicted Error by Discharge Current Limit",
    x = "Distance (km)",
    y = "Discharge Current Limit",
    fill = "Squared Error \n from Ideal \n Voltage"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 16), axis.title = element_text(size = 14) )


print(p_DCL)


# Save the plot
ggsave(
  filename = paste0("Code/RSM Plots/Ideal_Voltage_Diff_vs_DCL_UPPER_CIs.png"),
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
  distance = seq(from=0, to = max(data$distance), by=300),
  packccl = seq(from = min(data$packccl), to = max(data$packccl), by = .25),
) %>%
  mutate(packdcl = 30, #This is the mode
         avg_current = median(data$avg_current))%>%
  mutate(id = 1:n())

grid = grid %>%
  mutate(yhat = predict(m3_s, newdata = .),
         se = glance(m3_s)$sigma)

# Monte carlo sampling
grid = grid %>% 
  group_by(id) %>%
  reframe(
    ysim = rnorm(n = 10000, mean = yhat, sd = se),
    ysim = ysim^2
  ) %>%
  group_by(id) %>%
  summarize(
    upper = quantile(ysim, probs = 0.95)) %>%
  left_join(by = "id", y = grid)


# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = grid$yhat^2,
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
  
  # # Upper CI contours
  # geom_contour(
  #   data = grid,
  #   mapping = aes(x = distance / 1000, y = packccl, z = upper, color = "Upper 95% CI", linetype = "Upper 95% CI"),
  #   binwidth = 500,
  #   size = 0.5
  # ) +
  
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
    values = c("Upper 95% CI" = "dashed", "Smallest Error" = "solid")
  ) +
  scale_color_manual(
    name = " ",
    values = c("Upper 95% CI" = "grey", "Smallest Error" = "green2")
  ) +
  
  # Labels and themes
  labs(
    title = "RSM for CCL",
    subtitle = "Predicted Error by Charge Current Limit",
    x = "Distance (km)",
    y = "Charge Current Limit",
    fill = "Squared Error \n from Ideal \n Voltage"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 16), axis.title = element_text(size = 14) )


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
  distance = seq(from=0, to = max(data$distance), by=300),
  avg_current = seq(from = min(data$avg_current), to = max(data$avg_current), by = 1),
) %>%
  mutate(packdcl = 30, #This is the mode
         packccl = 25)%>%
  mutate(id = 1:n())

grid = grid %>%
  mutate(yhat = predict(m3_s, newdata = .),
         se = glance(m3_s)$sigma)

# Monte carlo sampling
grid = grid %>% 
  group_by(id) %>%
  reframe(
    ysim = rnorm(n = 10000, mean = yhat, sd = se),
    ysim = ysim^2
  ) %>%
  group_by(id) %>%
  summarize(
    upper = quantile(ysim, probs = 0.95)) %>%
  left_join(by = "id", y = grid)


# Get the grid with confidence interval
grid = grid %>% 
  mutate(
    yhat = grid$yhat^2,
  )
# Add contour tolerance
p_curr <- ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance / 1000, y = avg_current, fill = yhat)) +
  
  # Main contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
    color = "white", linetype = "solid",
    binwidth = 500,
    size = 0.5
  ) +

  # Upper CI contours
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = upper, color = "Upper 95% CI", linetype = "Upper 95% CI"),
    binwidth = 500,
    size = 0.5
  ) +
  
  # Smallest Error line
  geom_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat, color = "Smallest Error", linetype = "Smallest Error"),
    breaks = c(.01),
    size = 1
  ) +
  
  # Add text labels for contours
  geom_text_contour(
    data = grid,
    mapping = aes(x = distance / 1000, y = avg_current, z = yhat),
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
    values = c("Upper 95% CI" = "dashed", "Smallest Error" = "solid")
  ) +
  scale_color_manual(
    name = " ",
    values = c("Upper 95% CI" = "grey", "Smallest Error" = "green2")
  ) +
  
  # Labels and themes
  labs(
    title = "RSM for Average Current",
    subtitle = "Predicted Error by 30s Avg Current",
    x = "Distance (km)",
    y = "Average Current",
    fill = "Squared Error \n from Ideal \n Voltage"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 16), axis.title = element_text(size = 14) )


print(p_curr)

# Save the plot
ggsave(
  dpi = 300,
  width = 7,
  height = 5,
  filename = paste0("Code/RSM Plots/Ideal_Voltage_Diff_vs_Average Current_WithCIs.png"),
  plot = p_curr
)

bestcurrent = grid %>%
  filter(yhat == min(yhat))

# POSTER PLOT RSM ##############
install.packages("ggpubr")
library(ggpubr)
summaryp = ggpubr::ggarrange(plotlist = list(p_DCL,p_CCL,p_curr), 
                             common.legend = TRUE,legend = "none",
                             nrow=3,ncol=1 )

print(summaryp)

ggsave(
  dpi = 300,
  width = 6,
  height = 18,
  filename = paste0("Code/RSM Plots/POSTER_RSM_Vertical.png"),
  plot = summaryp
)

# Parameter Optimization Simple############
bestcurrent = bestcurrent$avg_current
bestcurrent
# 20.19853

bestdcl = bestdcl$packdcl
bestdcl
# 35

bestccl = bestccl$packccl
bestccl
# 26.5

grid = expand_grid(
  distance = c(5000,10000,15000)) %>% 
    mutate(packdcl = 35,
           packccl = 26.5,
           avg_current = bestcurrent) %>%
  mutate(id = 1:n())

grid = grid %>%
  mutate(yhat = predict(m3_s, newdata = .),
         se = glance(m3_s)$sigma)

# Monte carlo sampling
grid = grid %>% 
  group_by(id) %>%
  reframe(
    ysim = rnorm(n = 10000, mean = yhat, sd = se),
    ysim = ysim^2
  ) %>%
  group_by(id) %>%
  summarize(
    upper = quantile(ysim, probs = 0.95)) %>%
  left_join(by = "id", y = grid)

grid


# OPTIMAL PARAMS - Complex ########
# Use optimization to minimize yhat at t=5000,10000,15000

# Define the optimization function
optimize_params <- function(time, model) {
  # Define the objective function to minimize
  objective_function <- function(params) {
    newdata <- data.frame(
      distance = time,
      packdcl = params[1],
      packccl = params[2],
      avg_current = params[3]
    )
    predict(model, newdata = newdata)^2
  }
  
  # Initial guesses for parameters
  initial_values <- c(packdcl = 30, packccl = 40, avg_current = 10)
  
  # Perform optimization
  result <- optim(
    par = initial_values,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = c(15, 0, 0), # Set lower bounds for parameters
    upper = c(180, 30, 50) # Set upper bounds for parameters
  )
  
  # Return the optimized parameters and the minimum value
  return(c(
    distance = time,
    Optimal_packdcl = result$par[1],
    Optimal_packccl = result$par[2],
    Optimal_avg_current = result$par[3],
    Minimum_Value = result$value
  ))
}

# Set time points
time_points <- c(0, 5000, 10000, 15000)
# stop at 15000m since many races do not go below this amount

# Optimize for each time point and calculate confidence intervals
results <- do.call(rbind, lapply(time_points, function(t) optimize_params(t, m3_s)))

# Convert results to a tibble
results <- as.data.frame(results)

# Print results
print(results)

grid = tibble(
  distance = results$distance,
  packdcl = results$Optimal_packdcl.packdcl,
  packccl = results$Optimal_packccl.packccl,
  avg_current = results$Optimal_avg_current.avg_current
)%>%
  mutate(id = 1:n())

grid = grid %>%
  mutate(yhat = predict(m3_s, newdata = .),
         se = glance(m3_s)$sigma)

# Monte carlo sampling
grid = grid %>% 
  group_by(id) %>%
  reframe(
    ysim = rnorm(n = 10000, mean = yhat, sd = se),
    ysim = ysim^2
  ) %>%
  group_by(id) %>%
  summarize(
    upper = quantile(ysim, probs = 0.95)) %>%
  left_join(by = "id", y = grid)

grid
