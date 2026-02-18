library(MASS)
attach(Insurance)
library(leaps)
library(stats)
summary(Insurance)
#model init
reg1<-lm(Claims~Holders+Age+Group+District)
reg2<-lm(Claims~sqrt(Holders)+Age+Group+District)
plot(reg1$fitted.values, reg1$residuals)
abline(h=0)
plot(reg2$fitted.values, reg2$residuals)
abline(h=0)
AIC(reg1)
AIC(reg2)
#studentized resids
stud.reg<-rstudent(reg1)
plot(stud.reg)
abline(h=2.5)
abline(h=-2.5)
#identify(stud.reg)
(1:64)[abs(stud.reg)>2.5]

#leverage
summary(Insurance)
lev<-hatvalues(reg1)
thres<-(5/nrow(Insurance))*2.5
head(sort(lev, decr=T))

lev2<-hatvalues(reg2)
thres2<-(4/nrow(Insurance))*2.5
sort(lev2, decr=T)
#cooks disty
plot(cooks.distance(reg2))
identify(cooks.distance(reg2))

#dffitz
summary(reg2)
dfmod<-dffits(reg2)
plot(dffits(reg2))
dffits.thres<-2*sqrt(11/64)
dffits.thres
abline(h=dffits.thres)
abline(h=-dffits.thres)
(1:64)[dfmod>dffits.thres]

dfbeta(reg2)[8,]

################################################################################
################################################################################
################################################################################
install.packages("car")
library(car)
attach(Duncan)
summary(Duncan)

reg3<-lm(prestige~income+education+type)
summary(reg3)
#studentizeded
stud.reg2<-rstudent(reg3)
plot(stud.reg2)
abline(h=2.5)
abline(h=-2.5)
(1:45)[abs(stud.reg2)>2.5]
outliers <- which(abs(stud.reg2) > 2.5)
outliers
#identify(stud.reg2)

#leverage
lev3<-hatvalues(reg3)
thres3<-(5/nrow(Duncan))*2
thres3
lev3
sort(lev3, decr=T)
(1:45)[lev3>thres3]

#COOKS
plot(cooks.distance(reg3))
abline(h=4/nrow(Duncan))
(1:45)[cooks.distance(reg3)>(4/nrow(Duncan))]
#DFFITS
summary(reg3)
dfmod2<-dffits(reg3)
dffits(reg3)[which.max(abs(dffits(reg3)))]

plot(dffits(reg3))
dffits.thres2<-2*sqrt(length(coef(reg3))/nrow(Duncan))
dffits.thres2
abline(h=dffits.thres2)
abline(h=-dffits.thres2)
(1:45)[abs(dfmod2)>dffits.thres2]
#beta change
dfb<-dfbeta(reg3)
dfb



which.max(abs(dfb[,"income"]))
dfb[6,]
Duncan[6,]

which.max(abs(dfb[,"education"]))
dfb[17,]
Duncan[17,]

dfb <- dfbeta(reg3)  
dfb[6,]       


which.max(abs(dfb[,"(Intercept)"]))
dfb[19,]
Duncan[19,]

#########################################
##PCA
#If you use principal components to replace income and education with
#just one predictor, how much variability in income and education can be
#explained with just one term?

pcaIncome<-prcomp(Duncan[,c("income","education")], scale.=T)
summary(pcaIncome)
reg4 <- lm(prestige ~ income + education+type, data = Duncan)

regPCA<-lm(prestige~pcaIncome$x[,1]+type, data=Duncan)
summary(regPCA)
summary(reg4)

summary(regPCA)$adj.r.squared
summary(reg4)$adj.r.squared

AIC(regPCA)
AIC(reg4)
vif(reg4)
vif(regPCA)

