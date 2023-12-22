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

#1(c)
posthoc <- TukeyHSD(anova_model)
print(posthoc)

pairwise.t.test(vinegar_data$pH,vinegar_data$Site,p.adj="bonferroni")

# Perform one-way ANOVA
anova_result <- aov(vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data)

# Summary of ANOVA results
summary(anova_result)

# Perform pairwise t-tests
pairwise_result <- pairwise.t.test(vinegar_data$pH, vinegar_data$Site, p.adj = "none")
pairwise_result

#Anova analysis
grp = factor(vinegar_data$Site)
grp
y = vinegar_data$pH
y
drug.lm = lm(y ~ grp)
anova(drug.lm)

oneway.test(y ~ grp, var.equal = TRUE)

#1(c)
require(lawstat)
site = factor(vinegar_data$Site)

acidityLevels = vinegar_data$pH

levene.test(acidityLevels,site,location = "median")
levene.test(acidityLevels,site,location = "mean")
levene.test(acidityLevels,site)

# Fit the ANOVA model
anova_model <- aov(vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data)
print(anova_model)
# Obtain residuals
residuals <- residuals(anova_model)
print(residuals)

# Plot residuals against fitted values or group labels
plot(fitted(anova_model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

#1(d)
posthoc <- TukeyHSD(anova_model)
posthoc

#1(e)
p_values <- c(0.4765222, 0.3632112, 0.9923747, 0.0916315, 0.9998934, 0.2519132, 0.0015038, 0.1723996, 0.0006834, 0.2118486)
holm_corrected <- p.adjust(p_values, method = "holm")
holm_corrected


# Apply Bonferroni correction
bonferroni_corrected <- p.adjust(p_values, method = "bonferroni")

# Display the corrected p-values
bonferroni_corrected


#2(a)



# Boxplot of Venom Yield by Body Class and Expression
boxplot(`Yield (mg)` ~ `Body Class` * `Expression`, data = VenomYield,
        xlab = "Body Class and Expression", ylab = "Venom Yield (mg)",
        main = "Venom Yield Distribution by Body Class and Expression")


#2(b)

# Assuming 'VenomYield' is your dataframe with columns 'Yield (mg)', 'Body Class', and 'Expression'
model <- aov(`Yield (mg)` ~ `Body Class` * `Expression`, data = VenomYield)

# Perform the two-way ANOVA
summary(model)

#2(c)


# Running ANCOVA
ancova_result <- lm(`Yield (mg)` ~ `Expression` * `Body Length (cm)`, data = VenomYield)
summary(ancova_result)


# 3 (a)


# Define the hazard function
hazard <- function(t) {
  return(1 - exp(-t))
}

# Survival function
survival_function <- function(t) {
  return(exp(-integrate(hazard, lower = 0, upper = t)$value))
}

# Failure probability density function
failure_density <- function(t) {
  return(hazard(t) * survival_function(t))
}

# Create a sequence of time points
time_points <- seq(0, 10, by = 0.1)

# Calculate S(t) and f(t) for the time points
survival_values <- sapply(time_points, survival_function)
failure_density_values <- sapply(time_points, failure_density)

# Plotting the survival function S(t)
plot(time_points, survival_values, type = 'l', xlab = 'Time (years)', ylab = 'Survival Probability', main = 'Survival Function S(t)')

# Plotting the failure probability density function f(t)
plot(time_points, failure_density_values, type = 'l', xlab = 'Time (years)', ylab = 'Failure Density', main = 'Failure Probability Density Function f(t)')


#3 (b)

# Failure and censoring data
failures <- c(1, 0, 3, 4, 11, 8, 8, 15, 17, 10)
censoring <- c(0, 0, 2, 0, 0, 5, 3, 4, 2, 1)

# Calculate the total number of units (population)
total_units <- 100

# Calculate the remaining units at risk (accounts for censoring)
units_at_risk <- total_units - cumsum(censoring)

# Calculate the survival probability at each time point
survival_prob <- cumprod(1 - failures / units_at_risk)

# Calculate the hazard function
hazard <- failures / units_at_risk

# Calculate the failure probability density function
failure_density <- c(0, diff(failures) / units_at_risk[-length(units_at_risk)])

# Plot the survival function S(t)
plot(failures, survival_prob, type = "s", xlab = "Time (years)", ylab = "Survival Probability", main = "Survival Function S(t)")

# Plot the hazard function h(t)
plot(failures, hazard, type = "s", xlab = "Time (years)", ylab = "Hazard Rate", main = "Hazard Function h(t)")

# Plot the failure probability density function f(t)
plot(failures, failure_density, type = "s", xlab = "Time (years)", ylab = "Failure Density", main = "Failure Probability Density Function f(t)")
