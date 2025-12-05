library(MASS)
attach(Insurance)
data(Insurance)

## 1a
plot(Holders, Claims,
     xlab="Policyholders",
     ylab="Insurance Claims",
     pch=16)
mod1<-lm(Claims~Holders)
abline(mod1, col="royalblue", lwd=2)
plot(mod1$fitted.values, mod1$residuals)
abline(h=0, col=2, lwd=3)

## 1b
ins2<-Insurance[Insurance$Claims>0,]
plot(log(Holders), log(Claims),
     xlab="Policyholders",
     ylab="Insurance Claims",
     pch=16)
mod2 <- lm(log(Claims) ~ log(Holders), data = ins2)
abline(mod2, col="royalblue", lwd=2)
summary(mod2)
## 1d
plot(mod2$fitted.values, mod2$residuals, pch=16)
quantile(mod2$fitted.values, c(.2,.4,.6,.8))
fv<-mod2$fitted.values
grps<-(fv>4.133775 )+(fv>3.566796 )+(fv>2.773839 )+(fv>2.090657 )
table(grps)
plot(mod2$fitted.values, mod2$residuals, pch=16, col=grps+1)

grp.var<-by(mod2$residuals, grps, var)
grp.med<-by(mod2$fitted, grps, median)

#plot(grp.med, grp.var, pch=16)
#plot(grp.med^0.5, grp.var, pch=16)
plot(1/grp.med, grp.var, pch=16)
var.mod<-lm(grp.var~I(1/grp.med))
abline(var.mod, col="royalblue3", lwd=3)
var.est<-var.mod$coef[1]+(var.mod$coef[2]*mod2$fitted.values)

mod3<-lm(I(log(ins2$Claims))~I(log(ins2$Holders)),weights=1/var.est)
summary(mod3)$coef
summary(mod2)
summary(mod3)

## 1e
predict(mod2, newdata=data.frame(Holders=700), interval="confidence", level=0.98)
predict(mod2, newdata=data.frame(Holders=80), interval="prediction", level=0.98)


## 2a
library(MASS)
attach(muscle)
help(muscle)
#normal
plot(Conc, Length)
reg1<-lm(Length~Conc)
abline(reg1, col="darkred", lwd=3)
plot(reg1$fitted.values, reg1$residuals)
abline(h=0, col=2, lwd=3)
#squared
plot(Conc^2, Length)
reg2<-lm(Length~I(Conc^2))
abline(reg2, col="darkred", lwd=3)
plot(reg2$fitted.values, reg2$residuals)
abline(h=0, col=2, lwd=3)
#square root
windows()
plot(sqrt(Conc), Length)
reg3<-lm(Length~sqrt(Conc))
abline(reg3, col="darkred", lwd=3)
plot(reg3$fitted.values, reg3$residuals)
abline(h=0, col=2, lwd=3)
#inverse
plot(1/Conc, Length)
reg4<-lm(Length~I(1/Conc))
abline(reg4, col="darkred", lwd=3)
plot(reg4$fitted.values, reg4$residuals)
abline(h=0, col=2, lwd=3)
#logarithmic
windows()
plot(log(Conc), Length)
reg5<-lm(Length~log(Conc))
abline(reg5, col="darkred", lwd=3)
plot(reg5$fitted.values, reg5$residuals)
abline(h=0, col=2, lwd=3)

library(faraway)
attach(clot)
help(clot)
#plot(conc, time)

## 3a 
plot(conc[lot=="one"], time[lot=="one"])
blood1<-lm(time[lot=="one"]~conc[lot=="one"])
abline(blood1, col="darkred", lwd=3)
plot(blood1$fitted.values, blood1$residuals)
abline(h=0, col=2, lwd=3)
# squared
plot(I(conc[lot=="one"]^2), time[lot=="one"])
blood2<-lm(time[lot=="one"]~I(conc[lot=="one"]^2))
abline(blood2, col="darkred", lwd=3)
plot(blood2$fitted.values, blood2$residuals)
abline(h=0, col=2, lwd=3)
#sqrt
plot(sqrt(conc[lot=="one"]), time[lot=="one"])
blood3<-lm(time[lot=="one"]~sqrt(conc[lot=="one"]))
abline(blood3, col="darkred", lwd=3)
plot(blood3$fitted.values, blood3$residuals)
abline(h=0, col=2, lwd=3)
#inverse
plot(I(1/conc[lot=="one"]), time[lot=="one"])
blood4<-lm(time[lot=="one"]~I(1/conc[lot=="one"]))
abline(blood4, col="darkred", lwd=3)
plot(blood4$fitted.values, blood4$residuals)
abline(h=0, col=2, lwd=3)
#logarithmic
plot(log(conc[lot=="one"]), time[lot=="one"])
blood5<-lm(time[lot=="one"]~log(conc[lot=="one"]))
abline(blood5, col="darkred", lwd=3)
plot(blood5$fitted.values, blood5$residuals)
abline(h=0, col=2, lwd=3)

predict(blood4, newdata=data.frame(Conc=50), interval="confidence", level=0.95)
predict(blood4, newdata = data.frame(`I(1/conc[lot == "one"])` = 1/50), interval="confidence", level=0.95)


lot1 <- clot[lot=="one", ]
lot1$invConc <- 1/lot1$conc
blood4<-lm(time ~ invConc, data = lot1)
predict(blood4, newdata = data.frame(invConc = 1/50), interval="confidence", level=0.95)

install.packages("sp")
library(sp)
data(meuse)
attach(meuse)
library(MASS)

## Baseline plot
plot(dist, lead)
model1<-lm(lead~dist, data=meuse)
abline(model1, col="darkred", lwd=3)
plot(model1$fitted.values, model1$residuals)
abline(h=0, col=2, lwd=3)

#squared
plot(I(dist^2), lead)
model2<-lm(lead~I(dist^2))
abline(model2, col="darkred", lwd=3)
plot(model2$fitted.values, model2$residuals)
abline(h=0, col=2, lwd=3)
#square root
plot(sqrt(dist), lead)
model3<-lm(lead~sqrt(dist))
abline(model3, col="darkred", lwd=3)
plot(model3$fitted.values, model3$residuals)
abline(h=0, col=2, lwd=3)
#log
plot(log(dist), lead)
df<-meuse[dist > 0, ]
model4<-lm(lead ~ log(dist), data=df)
abline(model4, col="darkred", lwd=3)
plot(model4$fitted.values, model4$residuals)
abline(h=0, col=2, lwd=3)
#inverse
plot(I(1/dist), lead)
model5<-lm(lead~I(1/dist), data=df)
abline(model5, col="darkred", lwd=3)
plot(model5$fitted.values, model5$residuals)
abline(h=0, col=2, lwd=3)

#boxcox
bc<-boxcox(model1)
lambda <- bc$x[which.max(bc$y)]
lead_trans<-(lead^lambda-1)/lambda
model6<-lm(lead_trans~dist)
plot(dist, lead_trans)
abline(model6)
plot(model6$fitted.values, model6$residuals)
abline(h=0, col=2, lwd=3)

#confidence interval
predict(model3,list(dist=c(.25, .7, 1)),interval="confidence", level=0.95)
predict(model4,list(dist=c(.25, .7, 1)),interval="confidence", level=0.95)
conft<-predict(model6,list(dist=c(.25, .7, 1)),interval="confidence", level=0.95)
conft_norm<-(lambda * conft + 1)^(1/lambda)
conft_norm
