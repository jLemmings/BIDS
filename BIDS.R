library(readr)
soil <- read_csv("soil.csv")
# Task 1
View(soil)

# Task 2
str <- str(soil)
sum <- summary(soil)

# Task 3
print(sum)

#Alle Plots für CEC1
plot(CEC1 ~ Clay1, data=soil)
print(cor(soil$CEC1,soil$Clay1))

plot(CEC1 ~ Clay2, data=soil)
print(cor(soil$CEC1,soil$Clay2))

plot(CEC1 ~ Clay5, data=soil)
print(cor(soil$CEC1,soil$Clay5))

plot(CEC1 ~ OC1, data=soil)
print(cor(soil$CEC1,soil$OC1))

plot(CEC1 ~ OC2, data=soil)
print(cor(soil$CEC1,soil$OC2))

plot(CEC1 ~ OC5, data=soil)
print(cor(soil$CEC1,soil$OC5))

#Alle Plots für CEC2
plot(CEC2 ~ Clay1, data=soil)
print(cor(soil$CEC2,soil$Clay1))

plot(CEC2 ~ Clay2, data=soil)
print(cor(soil$CEC2,soil$Clay2))

plot(CEC2 ~ Clay5, data=soil)
print(cor(soil$CEC2,soil$Clay5))

plot(CEC2 ~ OC1, data=soil)
print(cor(soil$CEC2,soil$OC1))

plot(CEC2 ~ OC2, data=soil)
print(cor(soil$CEC2,soil$OC2))

plot(CEC2 ~ OC5, data=soil)
print(cor(soil$CEC2,soil$OC5))

#Alle Plots für CEC3
plot(CEC5 ~ Clay1, data=soil)
print(cor(soil$CEC5,soil$Clay1))

plot(CEC5 ~ Clay2, data=soil)
print(cor(soil$CEC5,soil$Clay2))

plot(CEC5 ~ Clay5, data=soil)
print(cor(soil$CEC5,soil$Clay5))

plot(CEC5 ~ OC1, data=soil)
print(cor(soil$CEC5,soil$OC1))

plot(CEC5 ~ OC2, data=soil)
print(cor(soil$CEC5,soil$OC2))

plot(CEC5 ~ OC5, data=soil)
print(cor(soil$CEC5,soil$OC5))

# Alte Plots Aufgabe 3:
plot(soil$CEC1, type="l", col="blue", xlab="Count", ylab="Clay", ylim = c(0,80))
lines(soil$Clay1, type="l", col="red")
lines(soil$OC1, type="l", col="green")
legend(30, 80, legend=c("CEC1", "Clay1", "OC1"), col=c("blue", "red", "green"), lty=1:1, cex=0.8)

plot(soil$CEC2, type="l", col="blue", xlab="Count", ylab="Clay", ylim = c(0,80))
lines(soil$Clay2, type="l", col="red")
lines(soil$OC2, type="l", col="green")
legend(35, 80, legend=c("CEC2", "Clay2", "OC2"), col=c("blue", "red", "green"), lty=1:1, cex=0.8)

plot(soil$CEC5, type="l", col="blue", xlab="Count", ylab="Clay", ylim = c(0,110))
lines(soil$Clay5, type="l", col="red")
lines(soil$OC5, type="l", col="green")
legend(20, 100, legend=c("CEC5", "Clay5", "OC5"), col=c("blue", "red", "green"), lty=1:1, cex=0.8)

# Task 4 Was ist mit linearer Regression gemeint (hier nur eine unabhängige Variable?)
plot(CEC1 ~ Clay1, data=soil)
cor(soil$CEC1,soil$Clay1)
(fit = lm(CEC1 ~ Clay1, data = soil))
plot(CEC1 ~ Clay1, data=soil) + abline(fit)

plot(CEC1 ~ OC1, data=soil)
cor(soil$CEC1,soil$OC1)
(fit = lm(CEC1 ~ OC1, data = soil))
plot(CEC1 ~ OC1, data=soil) + abline(fit)

plot(CEC2 ~ Clay2, data=soil)
cor(soil$CEC2,soil$Clay2)
(fit = lm(CEC2 ~ Clay2, data = soil))
plot(CEC2 ~ Clay2, data=soil) + abline(fit)

plot(CEC5 ~ Clay5, data=soil)
cor(soil$CEC5,soil$Clay5)
(fit = lm(CEC5 ~ Clay5, data = soil))
plot(CEC5 ~ Clay5, data=soil) + abline(fit)

plot(CEC2 ~ OC2, data=soil)
cor(soil$CEC2,soil$OC2)
(fit = lm(CEC2 ~ OC2, data = soil))
plot(CEC2 ~ OC2, data=soil) + abline(fit)

plot(CEC5 ~ OC5, data=soil)
cor(soil$CEC5,soil$OC5)
(fit = lm(CEC5 ~ OC5, data = soil))
plot(CEC5 ~ OC5, data=soil) + abline(fit)

# Task 5
fit1_1 = lm(CEC1 ~ Clay1, data = soil)
summary(fit1_1)$r.squared
fit1_2 = lm(CEC1 ~ OC1, data = soil)
summary(fit1_2)$r.squared
fit1_3 = lm(CEC1 ~ Clay2, data = soil)
summary(fit1_3)$r.squared
fit1_4 = lm(CEC1 ~ OC2, data = soil)
summary(fit1_4)$r.squared
fit1_5 = lm(CEC1 ~ Clay5, data = soil)
summary(fit1_5)$r.squared
fit1_6 = lm(CEC1 ~ OC5, data = soil)
summary(fit1_6)$r.squared

fit2_1 = lm(CEC2 ~ Clay1, data = soil)
summary(fit2_1)$r.squared
fit2_2 = lm(CEC2 ~ OC1, data = soil)
summary(fit2_2)$r.squared
fit2_3 = lm(CEC2 ~ Clay2, data = soil)
summary(fit2_3)$r.squared
fit2_4 = lm(CEC2 ~ OC2, data = soil)
summary(fit2_4)$r.squared
fit2_5 = lm(CEC2 ~ Clay5, data = soil)
summary(fit2_5)$r.squared
fit2_6 = lm(CEC2 ~ OC5, data = soil)
summary(fit2_6)$r.squared

fit3_1 = lm(CEC5 ~ Clay1, data = soil)
summary(fit3_1)$r.squared
fit3_2 = lm(CEC5 ~ OC1, data = soil)
summary(fit3_2)$r.squared
fit3_3 = lm(CEC5 ~ Clay2, data = soil)
summary(fit3_3)$r.squared
fit3_4 = lm(CEC5 ~ OC2, data = soil)
summary(fit3_4)$r.squared
fit3_5 = lm(CEC5 ~ Clay5, data = soil)
summary(fit3_5)$r.squared
fit3_6 = lm(CEC5 ~ OC5, data = soil)
summary(fit3_6)$r.squared

# Task 6
regression = lm(CEC5 ~ Clay5+OC5, data = soil)
regression$coefficients
summary(regression)

#Predictions
pred1 = forecast(regression, newdata=data.frame(Clay5 = soil$Clay1, OC5 = soil$OC1))
print(pred1)
pred2 <- forecast(regression, newdata=data.frame(Clay5 = soil$Clay2, OC5 = soil$OC2))
print(pred2)

#Berechnung R-Squared
pred1 <- pred1$mean
myDf <- as.data.frame(pred1)
regression = lm(pred1 ~ Clay5+OC5, data = soil)
summary(regression)$r.squared

pred2 <- pred2$mean
myDf <- as.data.frame(pred2)
regression = lm(pred2 ~ Clay5+OC5, data = soil)
summary(regression)$r.squared
regression$coefficients #y=0.052559*Clay5+4.714326*OC5+2.099813
# Ist der bessere Prädiktor

# Task 7
res = resid(regression)
plot(res, ylab="Residuals", xlab="CEC", main="Residuals") +abline(0,0)

# Task 8
pred3 = forecast(regression, newdata=data.frame(Clay5 = (soil$Clay2)*0, OC5 = soil$OC2))
print(pred3)
pred4 = forecast(regression, newdata=data.frame(Clay5 = (soil$Clay2)*0.7, OC5 = soil$OC2))
print(pred4)
