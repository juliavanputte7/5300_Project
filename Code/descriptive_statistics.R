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

# Read and Clean Data ###############
## Read in the data #########
data = read_csv("COMBINED_With_Ground_Speed_CLEANED_1Decimal (1).csv") %>% 
  setNames(nm = stringr::str_replace_all(tolower(names(.)), " ", "")) 

# Remove NAs
data <- data %>% filter(!is.na(distance))

# Remove Infs
data <- data %>% filter(!is.infinite(distance))

# Remove rows where distance is greater than 22km
data = data %>% filter(distance <= 22000)

# Remove columns we do not need (leftover from previous model where we use SOC)
data <- data %>% select(-idealsoc, -soc_delta)
data %>% glimpse()

# Convert gforce to absolute and get ideal voltage
data = data %>% mutate(gforcelat = abs(gforcelat),
                       gforcelong = abs(gforcelong),
                       distance_km = distance/1000)

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

##Sample calculation of each are provided here. See each section for the full list.
mean_gforcelong = mean(data$gforcelong)
median_gforcelong = median(data$gforcelong)
min_gforcelong = min(data$gforcelong)
max_gforcelong = max(data$gforcelong)
mode_gforcelong = as.numeric(names(sort(table(data$gforcelong), decreasing = TRUE))[1])

##Mean ##############################
mean_packdcl = mean(data$packdcl)

mean_packccl = mean(data$packccl)

mean_distacne = mean(data$distance_km)

mean_gforcelat = mean(data$gforcelat)

mean_batttemphi = mean(data$batttemphi)

mean_batttemplo = mean(data$batttemplo)

mean_groundspeed = mean(data$groundspeed)

mean_avg_current = mean(data$avg_current)

mean_avg_bmsdcvoltage = mean(data$avg_bmsdcvoltage)

mean_inv_commandtq = mean(data$inv_commandtq)

mean_idealv = mean(data$idealv)

##Median ############################

median_packdcl = median(data$packdcl)

median_packccl = median(data$packccl)

median_distacne = median(data$distance_km)

median_gforcelat = median(data$gforcelat)

median_batttemphi = median(data$batttemphi)

median_batttemplo = median(data$batttemplo)

median_groundspeed = median(data$groundspeed)

median_avg_current = median(data$avg_current)

median_avg_bmsdcvoltage = median(data$avg_bmsdcvoltage)

median_inv_commandtq = median(data$inv_commandtq)

median_idealv = median(data$idealv)

##Minimum ##############################

min_packdcl = min(data$packdcl)

min_packccl = min(data$packccl)

min_distacne = min(data$distance_km)

min_gforcelat = min(data$gforcelat)

min_batttemphi = min(data$batttemphi)

min_batttemplo = min(data$batttemplo)

min_groundspeed = min(data$groundspeed)

min_avg_current = min(data$avg_current)

min_avg_bmsdcvoltage = min(data$avg_bmsdcvoltage)

min_inv_commandtq = min(data$inv_commandtq)

min_idealv = min(data$idealv)

##Maximum ###################

max_packdcl = max(data$packdcl)

max_packccl = max(data$packccl)

max_distacne = max(data$distance_km)

max_gforcelat = max(data$gforcelat)

max_batttemphi = max(data$batttemphi)

max_batttemplo = max(data$batttemplo)

max_groundspeed = max(data$groundspeed)

max_avg_current = max(data$avg_current)

max_avg_bmsdcvoltage = max(data$avg_bmsdcvoltage)

max_inv_commandtq = max(data$inv_commandtq)

max_idealv = max(data$idealv)

# data %>% glimpse()

##Mode #################################
mode_packdcl = as.numeric(names(sort(table(data$packdcl), decreasing = TRUE))[1])
mode_packccl = as.numeric(names(sort(table(data$packccl), decreasing = TRUE))[1])
mode_distance_km = as.numeric(names(sort(table(data$distance_km), decreasing = TRUE))[1])
mode_batttemphi = as.numeric(names(sort(table(data$batttemphi), decreasing = TRUE))[1])
mode_batttemplo = as.numeric(names(sort(table(data$batttemplo), decreasing = TRUE))[1])
mode_groundspeed = as.numeric(names(sort(table(data$groundspeed), decreasing = TRUE))[1])
mode_avg_current = as.numeric(names(sort(table(data$avg_current), decreasing = TRUE))[1])
mode_avg_bmsdcvoltage = as.numeric(names(sort(table(data$avg_bmsdcvoltage), decreasing = TRUE))[1])
mode_inv_commandtq = as.numeric(names(sort(table(data$inv_commandtq), decreasing = TRUE))[1])
mode_idealv = as.numeric(names(sort(table(data$idealv), decreasing = TRUE))[1])
mode_gforcelat = as.numeric(names(sort(table(data$gforcelat),decreasing = TRUE))[1])
