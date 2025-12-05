install.packages("openintro")
library(openintro)
help(bac)

attach(bac)
plot(beers, bac)
reg<-lm(bac~beers)
abline(reg, col="blue")

confint(reg, level=0.95)
help(confint)

qt(.99, 14)
summary(reg)

predict(reg, list(beers=c(1,3,7)), interval="confidence")
predict(reg, list(beers=c(1,3,7)), interval="confidence", level=.98)
summary(bac)
help(qt)
help(predict)