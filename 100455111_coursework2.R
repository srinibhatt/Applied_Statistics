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

# Assuming 'vinegar_data' is your dataframe with 'Acidity' and 'Factory_Location' columns

# Calculate IQR for each factory location
iqr_values <- tapply(vinegar_data$pH, vinegar_data$Site,IQR)
View(iqr_values)

# Perform one-way ANOVA
anova_result <- aov(vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data)

# Summary of ANOVA results
summary(anova_result)

# Perform pairwise t-tests
pairwise_result <- pairwise.t.test(vinegar_data$pH, vinegar_data$Site, p.adj = "none")
pairwise_result



