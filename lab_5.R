library(lmtest)
salaries<
attach(salaries)
View(salaries)
#1a
plot(Experience,Salary)
#1b
reg1<-lm(Salary~Experience)
abline(reg1)
reg1$coef
summary(reg1)
plot(reg1$fitted.values, reg1$residuals)
abline(h=0, col="red")
#1c
plot(reg1$fitted.values, reg1$residuals)
abline(h=0, col=3, lwd=2)
#1d
plot(log(Experience), Salary)
reg2<-lm(Salary~log(Experience))
abline(reg2)
plot(log(Experience), reg1$residuals)
abline(h=0, col=3, lwd=2)

plot(I(Experience^2), Salary)
reg3<-lm(Salary~I(Experience^2))
abline(reg3)
plot(I(Experience^2), reg1$residuals)
abline(h=0, col=3, lwd=2)

plot(sqrt(Experience), Salary)
reg4<-lm(Salary~sqrt(Experience))
abline(reg4)
plot(sqrt(Experience), reg1$residuals)
abline(h=0, col=3, lwd=2)

plot(I(1/Experience), Salary)
reg5<-lm(Salary~I(1/Experience))
abline(reg5)
plot(I(1/Experience), reg1$residuals)
abline(h=0, col=3, lwd=2)

#plot(Experience-mean(Experience), Salary)
#1e
plot(Experience-mean(Experience), reg1$residuals)
plot(sqrt(Experience)-mean(sqrt(Experience)), reg1$residuals)
plot(log(Experience)-mean(log(Experience)), reg1$residuals)
plot((I(Experience^2)-mean(I(Experience)^2)), reg1$residuals)
plot((I(1/Experience)-mean(I(1/Experience))), reg1$residuals)
#1f
Experience_sq<-I(Experience^2)
reg6<-lm(Salary~Experience+Experience_sq, data=salaries)
summary(reg6)
#1g
plot(reg6$fitted.values, reg6$residuals)
#1h
shapiro.test(reg6$residuals)
group<-reg6$fitted.values>median(reg6$fitted.values)
var.test(reg6$residuals[group], reg6$residuals[!group])
#1i
pred<-predict(reg6, newdata<-data.frame(Experience=15, Experience_sq=15^2), interval="prediction", level=0.98)
pred[,"upr"] - pred[,"lwr"]
pred1<-predict(reg1, newdata<-data.frame(Experience=15), interval="prediction", level=0.98)
pred1[,"upr"] - pred1[,"lwr"]
pred
#2
wine<-read.table("latour.txt", header=TRUE)
library(car)
attach(wine)
names(wine)
mod1<-lm(Quality~EndofHarvest*Rain)
summary(mod1)
avPlot(mod1, "Rain")
avPlot(mod1, "EndofHarvest")
predict(mod1, newdata=data.frame(EndofHarvest=0, Rain=1))

#3
library("faraway")
attach(gala)
fara<-lm(Species~., data=gala)
#3a and 3b
summary(fara)
#3c
avPlots(fara)
avPlot(fara, "Endemics")
avPlot(fara, "Area")
avPlot(fara, "Elevation")
fara_reduc<-lm(Species~Endemics+Area+Elevation)

#3d
avPlots(fara_reduc)
crPlots(fara_reduc)
summary(fara_reduc)

#3e
fara_reduc_log<-lm(Species~Endemics+log(Area)+Elevation)
summary(fara_reduc_log)
#3f
avPlots(fara_reduc_log)
#3g
fara_reduc_log2<-lm(Species~Endemics+log(Area))
summary(fara_reduc_log2)

library("faraway")
library(car)
attach(sat)
help(sat)
#4a
satReg1<-lm(total~takers+salary+ratio+expend)
summary(satReg1)
#4b
avPlots(satReg1)
#4c
satReg2<-lm(total~takers+salary+ratio)
summary(satReg2)
avPlots(satReg2)
#4d
crPlots(satReg2)
#4e

satReg3<-lm(total~log(takers)+salary+ratio)
crPlots(satReg3)
par(mfrow=c(1,2))
plot(satReg3, which=1)
plot(satReg3, which=2)
summary(satReg3)

satReg2<-lm(total~takers+salary+ratio)
crPlots(satReg2)
plot(reg2$fitted.values, reg2$residuals, 
     xlab="Fitted Values",
     ylab="Residuals",
     cex=2)
satReg3<-lm(total~takers+salary+log(ratio))
crPlots(satReg3)
plot(reg3$fitted.values, reg3$residuals, 
     xlab="Fitted Values",
     ylab="Residuals",
     cex=2)
#4f
oregon<-sat["Oregon", ]
print(oregon)
predict(satReg2, newdata = oregon)
oregon$total
#4g
predict(satReg3)
sat$total
modelStates<-sat$total-predict(satReg3)
modelStates
sat[which.max(abs(modelStates)),]
#4h
sat[which.min(abs(modelStates)),]
#4i
coef(satReg1)
crPlots(satReg1)
