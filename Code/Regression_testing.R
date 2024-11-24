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
library(corrplot)

# Read in the data #########
data = read_csv("Data/COMBINED_With_Ground_Speed_CLEANED_1Decimal (1).csv") %>% 
  setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", "")) 

data <- data %>% filter(!is.na(distance))

# Get SOC delta (Squared Error)
data = data %>%
  mutate(soc_delta = (idealsoc - packsoc)^2 / 100)



# Plot SOC Curve ####
# Create the plot
ggplot(data, aes(x = distance / 1000, y = idealsoc)) +
  geom_line(color = "blue", size = 1, linetype = "solid") +
  
  # Adding label for the ideal SOC curve
  geom_text(aes(label = "Ideal SOC Curve"), 
            x = max(data$distance / 1000) * 0.8,  # Adjust x position
            y = max(data$idealsoc) * 0.9,        # Adjust y position
            color = "blue", size = 4, hjust = 0) +
  
  # Plot SOC curves for each unique date
  geom_line(data = data %>% group_by(logdate), 
            aes(x = distance / 1000, y = packsoc, color = as.factor(logdate)),
            size = 0.8) +
  
  labs(
    x = "Distance (km)",
    y = "SOC %",
    title = "SOC Curves for 8 Races vs Ideal",
    color = "Race Date"
  ) +
  scale_color_discrete(name = "Race Date") +
  theme_minimal()

# Plot Voltage Curve ####
# Create the plot
ggplot(data, aes(x = packsoc, y = idealv))+
  
  # Plot SOC curves for each unique date
  geom_point(data = data %>% group_by(logdate), 
             aes(x = packsoc, y = bmsdcvoltage, color = as.factor(logdate)),
             size = 0.8) +
  
  labs(
    x = "SOC %",
    y = "Voltage",
    title = "Voltage SOC Curves for 8 Races",
    color = "Race Date"
  ) +
  scale_x_reverse() +
  scale_color_discrete(name = "Race Date") +
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
corr_data = select(data, c(batttemphi, distance,batttemplo,inv_commandtq,groundspeed,bms_dccurrent,packdcl,packccl,gforcelat,gforcelong))
matrix = cor(corr_data)

corrplot(matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,diag=FALSE,) 

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
    title = "Scatter Plot of inv_commandtq vs. bms_dccurrent",
    x = "Inv Command TQ",
    y = "BMS DC Current"
  ) +
  annotate(
    "text", 
    x = Inf, y = Inf, 
    label = paste("Correlation:", correlation, "\nP-value:", p_value),
    hjust = 1.1, vjust = 1.5, 
    size = 4,
    color = "blue"
  )

# We will just use inv_command_tq since these are so highly correlated

# Linear Regression ######
m0 = data %>%
  lm(
    formula = 
      soc_delta ~ # Response variable
      poly(distance,4) +
      packdcl +
      #poly(distance,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      #poly(packdcl,2) + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      #packccl + 
      #gforcelat + 
      #gforcelong +  # Removing this does not affect the r^2 value
      poly(batttemphi,2) +
      #batttemplo + 
      #groundspeed #+
      +inv_commandtq  # Polynomial terms don't really work here
    #+ factor(logdate)    #+ factor(vehicle) # did not cause an increase in r-sq
  )
glance(m0)
# R-SQ is pretty good!
# m$residuals %>% hist()
tidy(m0)
glance(m0)


## Regression Plots #########

# Distance
data$distance_poly1 <- poly(data$distance, 4)[, 1]

ggplot(data, aes(x = distance_poly1, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5, size=.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Distance (Polynomial Term 1) (km)",
    y = "SOC Delta",
    title = "Observed Distance Poly1 vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

ggplot(data, aes(x = distance/1000, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5,size=.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Distance (km)",
    y = "SOC Delta",
    title = "Observed Distance vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# DCL 
# Calculate the first polynomial term for packdcl
data$packdcl_poly1 <- poly(data$packdcl, 2)[, 1]

ggplot(data, aes(x = packdcl_poly1, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Pack DCL (Polynomial Term 1)",
    y = "SOC Delta",
    title = "Observed Pack DCL Poly1 vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# Batttemphi
ggplot(data, aes(x = batttemphi, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Battery Temp High",
    y = "SOC Delta",
    title = "Observed Battery Temp High vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# Batttemplo
ggplot(data, aes(x = batttemplo, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Battery Temp Low",
    y = "SOC Delta",
    title = "Observed Battery Temp Low vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# ground speed
ggplot(data, aes(x = groundspeed, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Ground Speed",
    y = "SOC Delta",
    title = "Observed Ground Speed vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# torque
ggplot(data, aes(x = inv_commandtq, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Torque",
    y = "SOC Delta",
    title = "Observed Torque vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

#ccl
ggplot(data, aes(x = packccl, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Pack CCL",
    y = "SOC Delta",
    title = "Observed Pack CCL vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# gforce lat
ggplot(data, aes(x = gforcelat, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "G-Force Lateral",
    y = "SOC Delta",
    title = "Observed G-Force Lateral vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# gforce lon
ggplot(data, aes(x = gforcelong, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "G-Force Longitudinal",
    y = "SOC Delta",
    title = "Observed G-Force Longitudinal vs SOC Delta with Regression Line"
  ) +
  theme_minimal()

# TODO##

# RSM #########

#soc_delta ~ poly(distance, 4) + 
#poly(packdcl, 2) + 
# packccl + gforcelat + 
#poly(batttemphi, 2) + batttemplo + 
#groundspeed

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## DCL and Distance #####

grid = expand_grid(
  distance = seq(from = 0, to = 22000, by = 100),
  packdcl = seq(from = min(data$packdcl), to = 200, by = .1)
) %>%
  mutate(gforcelat = 1,
         batttemphi = 50,
         inv_commandtq = 50) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance, y = packdcl, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Pack DCL and Distance",
    x = "Distance",
    y = "Pack DCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat <= min(yhat)+min(yhat)*.01)

# Best Pack dcl is 0 and best pack ccl is 30

## Battery Temps #####

grid = expand_grid(
  distance = c(1000, 5000, 10000),
  batttemphi = seq(from = min(data$batttemphi), to = max(data$batttemphi), by = .1),
  batttemplo = seq(from = min(data$batttemplo), to = max(data$batttemplo), by = .1)
) %>%
  mutate(gforcelat = 1,
         packdcl = 0,
         packccl = 30,
         groundspeed = getmode(data$groundspeed),
         bms_dccurrent = 10) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = batttemplo, y = batttemphi, fill = yhat)) +
  facet_wrap(~distance) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Battery Temperature Low and Battery Temperature High",
    subtitle = "for Distance = 1000m, 5000m, 10000m",
    x = "Battery Temp Low",
    y = "Battery Temp High",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat == min(yhat))

# Best batt temp lo is 19 and best batt tekp hi is 50

## Distance and Speed #########
grid = expand_grid(
  distance = seq(from = 0, to = 22000, by = 100),
  groundspeed = seq(from = min(data$groundspeed), to = max(data$groundspeed), by = 1)
) %>%
  mutate(gforcelat = 1,
         packdcl = 0,
         packccl = 30,
         batttemphi = 50,
         batttemplo = 19,
         bms_dccurrent = 10) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance, y = groundspeed, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Distance and Ground Speed",
    x = "Distance",
    y = "Ground Speed",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat == min(yhat))


## Gforce and Distance
grid = expand_grid(
  distance = seq(from = 0, to = 22000, by = 100),
  gforcelat = seq(from = min(data$gforcelat), to = max(data$gforcelat), by = .1)
) %>%
  mutate(groundspeed = 5,
         packdcl = 0,
         packccl = 30,
         batttemphi = 50,
         batttemplo = 19,
         bms_dccurrent = 10) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance, y = gforcelat, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Distance and G Force",
    x = "Distance",
    y = "G Force",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat == min(yhat))

## Current and Distance #########
grid = expand_grid(
  distance = seq(from = 0, to = 22000, by = 100),
  bms_dccurrent = seq(from = min(data$bms_dccurrent), to = max(data$bms_dccurrent), by = 1)
) %>%
  mutate(groundspeed = 5,
         packdcl = 0,
         packccl = 30,
         batttemphi = 50,
         batttemplo = 19,
         gforcelat = 0) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance, y = bms_dccurrent, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Distance and BMS DC Current",
    x = "Distance",
    y = "BMS DC Current",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat == min(yhat))

## Summary Plot ########
grid = expand_grid(
  packdcl = c(0,50,100,150,200),
  distance = seq(from = 0, to = 22000, by = 100),
  groundspeed = seq(from = min(data$groundspeed), to = max(data$groundspeed), by = 1)
) %>%
  mutate(gforcelat = 0,
         packccl = 30,
         batttemphi = 50,
         batttemplo = 19,
         bms_dccurrent = 50) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = distance, y = groundspeed, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  facet_wrap(~packdcl) +
  labs(
    title = "Predicted Error by Distance and Ground Speed, with Optimized Parameters",
    subtitle = "For Pack DCL 0, 50, 100, 150, 200",
    x = "Distance",
    y = "Ground Speed",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  group_by(distance) %>% 
  filter(yhat == min(yhat))
