library(readr)
soil <- read_csv("soil.csv")
plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9))
# Task 1
View(soil)

# Task 2
str <- str(soil)
sum <- summary(soil)

# Task 3
print(sum)
sum$r.squared
plot(soil['Clay1'])
cor(soil$Clay1, soil$OC1)

fit=lm(data=soil)
summary(fit)
plot(fit)

plot(soil$Clay1, type="l", col="blue", xlab="Count", ylab="Clay")
lines(soil$Clay2, type="l", col="red")
lines(soil$Clay5, type="l", col="green")

plot(soil$CEC1, type="l", col="blue", xlab="Count", ylab="CEC")
lines(soil$CEC2, type="l", col="red")
lines(soil$CEC5, type="l", col="green")

plot(soil$OC1, type="l", col="blue", xlab="Count", ylab="OC")
lines(soil$OC2, type="l", col="red")
lines(soil$OC5, type="l", col="green")

# Task 4

# Task 5

# Task 6

# Task 7

# Task 8

