library(readr)
soil <- read_csv("soil.csv")
# Task 1
View(soil)

# Task 2
str <- str(soil)
sum <- summary(soil)

# Task 3 Was ist mit "against" gemeint? Was ist mit disskutieren gemeint?
print(sum)
#sum$r.squared
plot(soil['Clay1'])
cor(soil$Clay1, soil$OC1)

#fit=lm(data=soil)
#summary(fit)
#plot(fit)

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
# Test
plot(OC1 ~ Clay1, data=soil)
cor(soil$OC1,soil$Clay1)
(fit = lm(OC1 ~ Clay1, data = soil))
plot(OC1 ~ Clay1, data=soil) + abline(fit)
lines(soil$CEC1, type="l", col="red")
# End Test

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
fit = lm(CEC1 ~ Clay1, data = soil)
summary(fit)$r.squared
fit2 = lm(CEC1 ~ OC1, data = soil)
summary(fit2)$r.squared
#OC ist der bessere Prädiktor

fit3 = lm(CEC2 ~ OC2, data = soil)
summary(fit3)$r.squared
fit4 = lm(CEC2 ~ Clay2, data = soil)
summary(fit4)$r.squared
#Clay ist der bessere Prädiktor

fit5 = lm(CEC5 ~ Clay5, data = soil)
summary(fit5)$r.squared
fit6 = lm(CEC5 ~ OC5, data = soil)
summary(fit6)$r.squared
#Clay ist der bessere Prädiktor

# Task 6
regression = lm(CEC1 ~ Clay1+OC1, data = soil)
#Die lineare Gleichung lautet: y=0.06475*Clay1+2.16240*OC1+2.71960
regression$coefficients
summary(regression)

#Predictions Was sind die beiden Prädiktoren? Ebene 2 und Ebene 5?
pred1 = forecast(regression, newdata=data.frame(Clay1 = soil$Clay2, OC1 = soil$OC2))
print(pred1)
pred2 <- forecast(regression, newdata=data.frame(Clay1 = soil$Clay5, OC1 = soil$OC5))
print(pred2)

#Berechnung R-Squared
pred1 <- pred1$mean
myDf <- as.data.frame(pred1)
regression = lm(pred1 ~ Clay1+OC1, data = soil)
summary(regression)$r.squared

pred2 <- pred2$mean
myDf <- as.data.frame(pred2)
regression = lm(pred2 ~ Clay1+OC1, data = soil)
summary(regression)$r.squared

# Task 7
res = resid(regression)
plot(res, ylab="Residuals", xlab="CEC", main="Residuals") +abline(0,0)

# Task 8
pred1 = forecast(regression, newdata=data.frame(Clay1 = 0, OC1 = soil$OC2))
print(pred1)
