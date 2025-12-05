install.packages("faraway")
library("faraway")
install.packages("leaps")
library(leaps)
attach(fat)
#1a
mod1<-lm(density~1)
summary(mod1)
sse1<-sum(resid(mod1)^2)
sse1
#1b
mod2<-lm(density~height+weight+age+neck+chest+
           abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
summary(mod2)
sse2<-sum(resid(mod2)^2)
sse2
#1c
forward<-step(mod1, scope=list(lower=mod1, upper=mod2), 
     direction="forward", k=2)
forward
#1d
stepwise<-step(mod1, scope=list(lower=mod1, upper=mod2),
               direction="both", k=2)
stepwise
#1e
backward<-step(mod2, scope=list(lower=mod1, upper=mod2),
               direction="backward",  k=2)
backward
#1f
vif(mod2)
mod3<-lm(density~height+age+neck+chest+
           knee+ankle+biceps+forearm+wrist)
vif(mod3)
#1g
elimination<-step(mod3, scope=list(lower=mod1, upper=mod3),
               direction="backward",  k=2)
elimination
#1h
bestMod<-regsubsets(density~height+weight+age+neck+chest+
                      abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data=fat, nvmax=6)
summary(bestMod)

mod4<-lm(density~weight+abdom+forearm+wrist)
mod5<-lm(density~weight+abdom+biceps+forearm+wrist)
mod6<-lm(density~weight+neck+abdom+biceps+forearm+wrist)
mod4
mod5
mod6

summary(mod6)
AIC(mod4)
AIC(mod5)
AIC(mod6)
AIC(backward)
AIC(forward)
AIC(stepwise)
AIC(elimination)

plot(mod6$fitted.values, mod6$residuals)
abline(h=0, col=2)
group<-mod6$fitted.values>median(mod6$fitted.values)
var.test(mod6$residuals[group], mod6$residuals[!group])

qqnorm(mod6$residuals, cex=2)
qqline(mod6$residuals, col=2, lwd=3)

plot(elimination$fitted.values, elimination$residuals)


#Question 2
wine<-read.csv("winequality-red.csv")
attach(wine)
reg1<-lm(quality~1)
reg2<-lm(quality~., data=wine)

forwardWine<-step(reg1, scope=list(lower=reg1, upper=reg2),
                  direction="forward", k=log(nrow(wine)))
forwardWine

backwardsWine<-step(reg2, scope=list(lower=reg1, upper=reg2),
                    direction="backward", k=log(nrow(wine)))
backwardsWine

bestWine<-regsubsets(quality~., data=wine, nbest=1, nvmax=11)
summary(bestWine)$bic
summary(bestWine)

reg3<-lm(quality~volatile.acidity+chlorides+total.sulfur.dioxide+pH+sulphates+alcohol)
reg3

stepwiseWine<-step(reg1, scope=list(lower=reg1, upper=reg3),
                   direction="both", k=log(nrow(wine)))
stepwiseWine

reg4<-lm(quality~volatile.acidity+chlorides+total.sulfur.dioxide+pH+sulphates+alcohol+citric.acid)
reg4

plot(stepwiseWine$fitted.values, stepwiseWine$residuals)
abline(h=0, col=2)
group<-stepwiseWine$fitted.values>median(stepwiseWine$fitted.values)
var.test(stepwiseWine$residuals[group], stepwiseWine$residuals[!group])

qqnorm(stepwiseWine$residuals, cex=2)
qqline(stepwiseWine$residuals, col=2, lwd=3)

########
predicted1<-predict(forwardWine)
difference1<-quality-predicted1
which.min(difference1)

predicted2<-predict(backwardsWine)
difference2<-quality-predicted2
which.min(difference2)

predicted3<-predict(reg3)
difference3<-quality-predicted3
which.min(difference3)

predicted4<-predict(stepwiseWine)
difference4<-quality-predicted4
which.min(difference4)
##########

predicted5<-predict(forwardWine)
difference5<-quality-predicted5
which.max(difference5)

predicted6<-predict(backwardsWine)
difference6<-quality-predicted6
which.max(difference6)

predicted7<-predict(reg3)
difference7<-quality-predicted7
which.max(difference7)

predicted8<-predict(stepwiseWine)
difference8<-quality-predicted8
which.max(difference8)
