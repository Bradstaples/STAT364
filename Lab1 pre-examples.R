library(openintro)
summary(bac)

plot(bac$beers, bac$bac, xlab="Beers", ylab="Blood Alcohol")

## Create median-median line
attach(bac)
med_beer<-median(beers)
abline(v=5, col="dodgerblue", lty=3, lwd=3)

grp1<-beers<med_beer

med_beer1<-median(beers[grp1])
med_beer2<-median(beers[!grp1])
abline(v=med_beer1, col="green4", lwd=2)
abline(v=med_beer2, col="green4", lwd=2)

med_bac1<-median(bac[grp1])
med_bac2<-median(bac[!grp1])
abline(h=med_bac1, col="orange", lwd=2)
abline(h=med_bac2, col="orange", lwd=2)

points(c(med_beer1, med_beer2), c(med_bac1, med_bac2),
       pch=16, cex=2, col="orchid3")

rise<-med_bac2-med_bac1
run<-med_beer2-med_beer1
slope<-rise/run

intercept<-med_bac1-(slope*med_beer1)
abline(intercept, slope, col="orchid", lwd=3)

## Get a line using least-squares regression

mod1<-lm(bac~beers)
abline(mod1, col="chartreuse3", lwd=2)

## compare lines
mod1$coefficients
c(intercept, slope)

## get SSTotal for BAC
SST<-sum((bac-mean(bac))^2)

## get SSE for each line
mod_median_resid<-bac-(intercept+(slope*beers))
SSE_median<-sum(mod_median_resid^2)
mod_lm_resid<-bac-(mod1$coef[1]+(mod1$coef[2]*beers))
SSE_lm<-sum(mod_lm_resid^2)

## R-square for each line
mod_median_r<-1-(SSE_median/SST)
mod_lm_r<-1-(SSE_lm/SST)

# you can also get the r-square from the lm model by:
summary(mod1)$r.square

# and the residuals could have been pulled using:
mod1$residuals

