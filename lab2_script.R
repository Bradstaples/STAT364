install.packages("openintro")
library(openintro)
help(bac)

attach(bac)
plot(beers, bac)
reg<-lm(bac~beers)
abline(reg, col="blue")

confint(reg, level=0.95)
help(confint)

qt(.01, 14)
qt(.05,14)

predict(reg, list(beers=c(1,3,7)), interval="confidence")
bac_data<-predict(reg, list(beers=c(1,3,7)), interval="prediction", level=0.9)
bac_data
bac_data[1, "upr"] - bac_data[1, "lwr"]

bac_data1<-predict(reg, list(beers=c(1)), interval="confidence", level=0.98)
bac_data2<-predict(reg, list(beers=c(4)), interval="confidence", level=0.95)
bac_data1[1, "upr"] - bac_data1[1, "lwr"]
bac_data2[1, "upr"] - bac_data2[1, "lwr"]
summary(bac)
help(qt)
#Lab work
predict(reg, list(beers=c(1,3,7)), interval="confidence", level=0.98)
predict(reg, list(beers=c(1,3,5,7)), interval="prediction", level=0.9)

library(MASS)
data(package="MASS")
help(hills)
attach(hills)
plot(dist, time)
reg2<-lm(time~dist)
abline(reg2, col="blue")
coef(reg2)
confint(reg2, level=0.9)
confint(reg2, level=0.95)
summary(reg2)
abs(qt(0.05,33))

width<-predict(reg2,list(dist=c(5)),interval="prediction", level=0.95)
width
width[1,"upr"]-width[1,"lwr"] 
width2<-predict(reg2,list(dist=c(5)),interval="confidence", level=0.95)
width2[1,"upr"]-width2[1,"lwr"]

qt(0.05,33)

#plot(priv~govt, data=govt_data)
#plot(priv~govt, data=brinks_data)

attach(meter_data)
govt_data<-subset(meter_data, brinks==0)
brinks_data<-subset(meter_data, brinks==1)


plot(priv~govt, data=meter_data)

govt_reg<-lm(priv~govt, data=govt_data)
brinks_reg<-lm(priv~govt, data=brinks_data)

summary(govt_reg)$coefficients
summary(brinks_reg)$coefficients

abline(govt_reg)
abline(brinks_reg)
sum(brinks_data$priv)
sum(govt_data$priv)
sum(predict(govt_reg, newdata = brinks_data))
sum(predict(govt_reg, newdata = brinks_data))-sum(brinks_data$priv)

confint(govt_reg, level=0.9)
confint(brinks_reg, level=0.9)
