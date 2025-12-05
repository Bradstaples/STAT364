install.packages("faraway")


                              ##question 1
library(faraway)
help(gala)
attach(gala)
#plotting the data and regression line
windows()
plot(Endemics, Species)
reg1<-lm(Species~Endemics)
abline(reg1, col="red", lwd=2)
#fitted vs residual, var.test
windows()
plot(reg1$fitted.values, reg1$residuals, cex=2)
abline(h=0, col=2, lwd=3)
group1<-reg1$fitted.values>median(reg1$fitted.values)
var.test(reg1$residuals[group1], reg1$residuals[!group1])

#qq test, sharpiro.test
qqnorm(reg1$residuals, cex=2)
qqline(reg1$residuals, col=2, lwd=3)
shapiro.test(reg1$residuals)


                        ##question 2
library(datasets)
help(Orange)
attach(Orange)
#plot and regression
windows()
plot(circumference, age, xlab="Circumference", ylab="Age")
reg2<-lm(age~circumference)
abline(reg2, col="red", lwd=2)

#fitted vs residual, var.test
windows()
plot(reg2$fitted.values, reg2$residuals, cex=2)
abline(h=0, col=2, lwd=3)
group2<-reg2$fitted.values>median(reg2$fitted.values)
var.test(reg2$residuals[group2], reg2$residuals[!group2])

#qq test, sharpiro.test
windows()
qqnorm(reg2$residuals, cex=2)
qqline(reg2$residuals, col=2, lwd=3)
shapiro.test(reg2$residuals)

#index vs residuals
windows()
plot(reg2$residuals, pch=16, cex=1.5)
circumference[5:15]
circumference[6:21]

                          ##question 3
library(datasets)
help(penguins)
penguins_df<-penguins[penguins$species=="Gentoo",]
attach(penguins_df)

#plot and regression
windows()
plot(flipper_len, body_mass)
reg3<-lm(body_mass~flipper_len)
abline(reg3, col="red", lwd=2)

#fitted vs residual, var.test
windows()
plot(reg3$fitted.values, reg3$residuals, cex=2)
abline(h=0, col=2, lwd=3)
group3<-reg3$fitted.values>median(reg3$fitted.values)
var.test(reg3$residuals[group3], reg3$residuals[!group3])

#qq test, sharpiro.test
windows()
qqnorm(reg3$residuals, cex=2)
qqline(reg3$residuals, col=2, lwd=3)
shapiro.test(reg3$residuals)

#index vs residuals
windows()
plot(reg3$residuals, pch=16, cex=1.5)
library(lmtest)
dwtest(reg3)
