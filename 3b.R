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
