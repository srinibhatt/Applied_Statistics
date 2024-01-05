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

shapiro.test(ph ~ Site, data= vinegar_data)
anova_model <- aov(formula = pH ~ Site, data = vinegar_data)

# Extracting residuals from the ANOVA model
residuals <- residuals(anova_model)

shapiro.test(residuals)
# Normal Q-Q plot for residuals
qqnorm(residuals)
qqline(residuals)
site = factor(vinegar_data$Site)

acidityLevels = vinegar_data$pH

levene.test(acidityLevels,site,location = "median")
levene.test(acidityLevels,site,location = "mean")
levene.test(acidityLevels,site)

# Fit the ANOVA model
anova_model <- aov(vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data)
print(anova_model)
summary(anova_model)
# Obtain residuals
residuals <- residuals(anova_model)
print(residuals)

# Plot residuals against fitted values or group labels
plot(anova_model,which=1 )

#1(d)
posthoc <- TukeyHSD(anova_model)
posthoc

aov(formula = vinegar_data$pH ~ vinegar_data$Site, data = vinegar_data)

#1(e)
p_values < - posthoc$`vinegar_data$Site`[, "p adj"]
print(p_values)
holm_corrected <- p.adjust(posthoc$`vinegar_data$Site`[, "p adj"], method = "holm")
holm_corrected

# Apply Bonferroni correction
bonferroni_corrected <- p.adjust(posthoc$`vinegar_data$Site`[, "p adj"], method = "bonferroni")

# Display the corrected p-values
bonferroni_corrected

correction_results = data.frame(original_values  = p_values, Holm_correction = holm_corrected, bonferroni_corrected = bonferroni_corrected)

print(correction_results)




#2(a)

print(VenomYield)
summary(VenomYield)
# Boxplot of Venom Yield by Body Class and Expression
boxplot(`Yield (mg)` ~ `Body Class` * `Expression`, data = VenomYield,
        xlab = "Body Class and Expression", ylab = "Venom Yield (mg)",
        main = "Venom Yield Distribution by Body Class and Expression")

interaction.plot(data = VenomYield,VenomYield$`Yield (mg)` ~ VenomYield$`Body Length (cm)`)
# Load necessary libraries




#2(b)
printVenomYield
# Assuming 'VenomYield' is your dataframe with columns 'Yield (mg)', 'Body Class', and 'Expression'
model <- aov(`Yield (mg)` ~ `Body Class` * `Expression`, data = VenomYield)

# Perform the two-way ANOVA
summary(model)

#2(c)


# Running ANCOVA
ancova_result <- lm(`Yield (mg)` ~ `Expression` * `Body Length (cm)`, data = VenomYield)
summary(ancova_result)


# 3 (a)

library(survival)

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

# Given data
time_intervals <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
ninint <- 100
nlost <- c(1, 0, 3, 4, 11, 8, 8, 15, 17, 10)
nevent <- c(0, 0, 2, 0, 0, 5, 3, 4, 2, 1)

# Generating the life table
my_table <- lifetab(time_intervals, ninint, nlost, nevent)

print(my_table)

# Extracting S, f, and h
S <- my_table[, 5]
f <- my_table[, 6]
h <- my_table[, 7]

# Adjusted time intervals
t <- 0.5 + c(0:9)

# Setting up a single plot area to combine all plots
par(mfrow = c(3, 1))  # 3 rows, 1 column

# Plotting all functions in one figure
plot(t, S, type = 'l', col = 'blue', xlab = 'Time (years)', ylab = 'Survival Probability', main = 'Survival Function S(t)')

plot(t, f, type = 'l', col = 'green', xlab = 'Time (years)', ylab = 'Failure Probability Density', main = 'Failure Density Function f(t)')

plot(t, h, type = 'l', col = 'red', xlab = 'Time (years)', ylab = 'Hazard Function', main = 'Hazard Function h(t)')


#

