# Load the necessary library
library(ggplot2)

source("Code/ideal_voltage_function.R")

# Generate the data
distance <- seq(0, 22, by = 1)

# Define the no_finish function
no_finish <- function(x) {
  return(ideal_voltage(x))
}

# Calculate Battery SOC (State of Charge) for all scenarios
Soc_ideal <- ideal_voltage(distance)
distance_no_finish = c(0,2,4,6,8,10,11,12,13,14,15,16,17,18,19,20,21,22)
Soc_no_finish <- ideal_voltage(distance_no_finish)  

distance_poor_utilization = seq(0,15,by=1)
Soc_poor_utilization <- ideal_voltage(distance_poor_utilization)

# Create a data frame for plotting
data_ideal <- data.frame(distance = distance, SOC = Soc_ideal, Type = "Ideal")
data_no_finish <- data.frame(distance = seq(0, 17, by = 1), SOC = Soc_no_finish, Type = "No Finish")
data_poor_utilization <- data.frame(distance = c(0,2,4,6,8,10,12,13,14,16,17,18,19,20,21,22), SOC = Soc_poor_utilization, Type = "Poor Utilization")

# Combine all data frames
data <- rbind(data_ideal, data_no_finish, data_poor_utilization)

ggplot(data) +
  geom_line(aes(x = distance, y = SOC, color = Type, linetype = Type), size = 1) +
  scale_color_manual(values = c("Ideal" = "forestgreen", 
                                "No Finish" = "firebrick", 
                                "Poor Utilization" = "goldenrod")) +
  scale_linetype_manual(values = c("Ideal" = "solid", 
                                   "No Finish" = "dashed", 
                                   "Poor Utilization" = "dashed")) +
  labs(x = "Distance (km)", y = "Voltage (V)", 
       title = "Ideal vs No-Finish vs Poor Utilization Voltage Discharge") +
  theme_minimal() +
  # Add vertical and horizontal lines with annotations
  geom_vline(xintercept = 22, linetype = "dotdash", color = "black", size = 0.5) +
  geom_hline(yintercept = min(data$SOC), linetype = "dotted", color = "darkred") +  # Fixed linetype
  geom_hline(yintercept = max(data$SOC), linetype = "dotted", color = "steelblue") +  # Fixed linetype
  # Annotate the beginning of the race
  annotate("text", x = 0, y = max(data$SOC)+2, label = "Start: Maximum Voltage", color = "steelblue", hjust = -0.1) +
  # Annotate the end of the ideal race
  annotate("text", x = 0, y = min(data$SOC)+2, label = "End: Minimum Voltage (Battery Dead)", color = "darkred", hjust = -0.1) +
  # Annotate the end of the race
  annotate("text", x = 22.3, y = 350, label = "End Of Race", color = "black", vjust = 0.5, angle=90) +
  # Add arrow indicating the squared distance
  geom_segment(aes(x = 10, y = 361, xend = 10, yend = 342), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_segment(aes(x = 12.5, y = 352, xend = 12.5, yend = 374), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  annotate("text", x = 10, y = 390, 
           label = "Quality metric:\nSquared distance\nfrom ideal curve", 
           color = "red", hjust = 0, size = 4)
  


