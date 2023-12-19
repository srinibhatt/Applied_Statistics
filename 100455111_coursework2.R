# Replace 'path_to_file' with the actual file path if the file is not in the working directory
vinegar_data <- vinegar

# Boxplot to visualize acidity differences between factory locations
boxplot(vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data, 
        xlab = "Factory Location", ylab = "Acidity",
        main = "Acidity Levels across Factory Locations")
# Calculate quartiles Q1 and Q3
q1 <- quantile(vinegar_data$pH, 0.25)
q3 <- quantile(vinegar_data$pH, 0.75)

# Calculate IQR manually
iqr_value <- q3 - q1
