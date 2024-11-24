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
corr_data = select(data, c(batttemphi, time,batttemplo,inv_commandtq,groundspeed,bms_dccurrent,packdcl,packccl,gforcelat,gforcelong))
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
      poly(time,4) + # Tried out a few polynomial terms for distance, this caused the highest r-sq
      packdcl + # Tried out a few polynomial terms for packdcl, this caused the highest r-sq
      packccl + 
      gforcelat + 
      #gforcelong +  # Removing this does not affect the r^2 value
      poly(batttemphi,2) +
      batttemplo + 
      groundspeed +
      inv_commandtq  # Polynomial terms don't really work here
    #+ factor(logdate)    #+ factor(vehicle) # did not cause an increase in r-sq
  )
glance(m0)
# R-SQ is pretty good!
# m$residuals %>% hist()
tidy(m0)
glance(m0)


## Regression Plots #########

# Time
ggplot(data, aes(x = time, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5,size=.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "Time (s)",
    y = "SOC Delta",
    title = "Observed Time vs SOC Delta with Regression Line"
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

# current
ggplot(data, aes(x = bms_dccurrent, y = soc_delta)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
  labs(
    x = "BMS DC Current",
    y = "SOC Delta",
    title = "Observed BMS DC Current vs SOC Delta with Regression Line"
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

grid = expand_grid(
  time = c(500,1000,2000),
  packdcl = seq(from = min(data$packdcl), to = 200, by = .1),
  packccl = seq(from = min(data$packccl), to = 30, by = 1)
) %>%
  mutate(gforcelat = 1,
         batttemphi = 30,
         batttemplo = 20,
         groundspeed = 15,
         inv_commandtq = 50) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = packdcl, y = packccl, fill = yhat)) +
  facet_wrap(~time) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Pack DCL and Pack CCL",
    subtitle = "for time = 500s, 1000s, 2000s",
    x = "Pack DCL",
    y = "Pack CCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

## DCL #####
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  packdcl = seq(from = min(data$packdcl), to = max(data$packdcl), by = 1),
) %>%
  mutate(gforcelat = 1,
         packccl = 25,
         batttemphi = 35,
         batttemplo = 30,
         groundspeed = 15,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = packdcl, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Pack DCL",
    x = "Time(s)",
    y = "Pack DCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  filter(yhat <= min(yhat)+min(yhat)*.01)

# Want to minimize pack DCL, at least less than 50

## CCL #####
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  packccl = seq(from = min(data$packccl), to = max(data$packccl), by = 1),
) %>%
  mutate(gforcelat = 1,
         packdcl = 25,
         batttemphi = 35,
         batttemplo = 30,
         groundspeed = 15,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = packccl, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Pack CCL",
    x = "Time(s)",
    y = "Pack CCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


grid %>%
  filter(yhat <= min(yhat)+min(yhat)*.01)

# Want to minimize pack DCL, at least less than 50

## Battery Temps #####

### High #####
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  batttemphi = seq(from = min(data$batttemphi), to = max(data$batttemphi), by = .1),
) %>%
  mutate(gforcelat = 1,
         packdcl = 30,
         packccl = 30,
         batttemplo = 30,
         groundspeed = 15,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = batttemphi, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Battery Temp High",
    x = "Time(s)",
    y = "Battery Temp High",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

### Low #######
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  batttemplo = seq(from = min(data$batttemplo), to = max(data$batttemplo), by = .1),
) %>%
  mutate(gforcelat = 1,
         packdcl = 30,
         packccl = 30,
         batttemphi = 50,
         groundspeed = 15,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = batttemplo, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Battery Temp Low",
    x = "Time(s)",
    y = "Battery Temp Low",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

## Gforcelat ########
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  gforcelat = seq(from = min(data$gforcelat), to = max(data$gforcelat), by = .1),
) %>%
  mutate(batttemplo = 19,
         packdcl = 30,
         packccl = 30,
         batttemphi = 50,
         groundspeed = 15,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = gforcelat, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by G Force",
    x = "Time(s)",
    y = "G Force Lat",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()


## Ground Speed ########
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  groundspeed = seq(from = min(data$groundspeed), to = max(data$groundspeed), by = .5),
) %>%
  mutate(batttemplo = 19,
         packdcl = 30,
         packccl = 30,
         batttemphi = 50,
         inv_commandtq = 20,
         gforcelat = 0) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = groundspeed, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Ground Speed",
    x = "Time(s)",
    y = "Ground Speed",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

## Torque ########
grid = expand_grid(
  time = seq(from=0, to = max(data$time), by=1),
  inv_commandtq = seq(from = min(data$inv_commandtq), to = max(data$inv_commandtq), by = 1),
) %>%
  mutate(batttemplo = 19,
         packdcl = 30,
         packccl = 30,
         batttemphi = 50,
         groundspeed = 10,
         gforcelat = 0) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = inv_commandtq, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma") +
  labs(
    title = "Predicted Outcome by Torque",
    x = "Time(s)",
    y = "Torque",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

## Summary Plot ########

# Pack CCL and Pack DCL
grid = expand_grid(
  packdcl = c(1,50,100,150,200),
  time = seq(from=0, to = max(data$time), by=10),
  packccl = seq(from=min(data$packccl), to= max(data$packccl), by=1)
) %>%
  mutate(gforcelat = .1,
         groundspeed = 10,
         batttemphi = 50,
         batttemplo = 19,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = packccl, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  facet_wrap(~packdcl) +
  labs(
    title = "Predicted Error by DCL and CCL, with Optimized Parameters",
    subtitle = "For Pack DCL 1, 50, 100, 150, 200",
    x = "Time",
    y = "CCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

# Pack CCL and Pack DCL
grid = expand_grid(
  packccl = c(1,10,15,20,30),
  time = seq(from=0, to = max(data$time), by=10),
  packdcl = seq(from=min(data$packdcl), to= max(data$packdcl), by=1)
) %>%
  mutate(gforcelat = .1,
         groundspeed = 10,
         batttemphi = 50,
         batttemplo = 19,
         inv_commandtq = 20) %>%
  mutate(yhat = predict(m0, newdata = tibble(.) ) %>% plogis()  )

ggplot() +
  geom_tile(data = grid, mapping = aes(x = time, y = packdcl, fill = yhat)) + 
  viridis::scale_fill_viridis(option = "plasma") +
  facet_wrap(~packccl) +
  labs(
    title = "Predicted Error by DCL and CCL, with Optimized Parameters",
    subtitle = "For Pack CCL 1, 10, 15, 20, 30",
    x = "Time",
    y = "DCL",
    fill = "Predicted Value (yhat)"
  ) +
  theme_minimal()

