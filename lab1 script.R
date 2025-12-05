#Labwork
library(openintro)
summary(bac)
attach(bac)
med_beer<-median(beers)

grp1<-beers<med_beer
table(grp1)
plot(beers, bac, xlab="beers", ylab="Blood Alcohol")
med_beer1<-median(beers[grp1])
med_beer2<-median(beers[!grp1])
med_bac1<-median(bac[grp1])
med_bac2<-median(bac[!grp1])

abline(v=med_beer1, col="green")
abline(v=med_beer2, col="green")
abline(h=med_bac1, col="orange")
abline(h=med_bac2, col="orange")

points(c(med_beer1, med_beer2), c(med_bac1, med_bac2), pch=16, cex=2, col = "orchid")

rise<-med_bac2-med_bac1
run<-med_beer2-med_beer1
slope<-rise/run

intercept <-med_bac1-(slope*med_beer1)
abline(intercept, slope, col="orchid")

mod1<-lm(bac~beers)
abline(mod1)

mod1$coefficients
abline(h=mean(bac), lwd=2)

sst<-sum((bac-mean(bac))^2)

mod_median_resid <- bac-(intercept+(slope*beers))
#HW Question 1

help(USArrests)
plot(USArrests$Assault, USArrests$Murder, xlab="Assault", ylab="Murder")

identify()
identify(USArrests$Assault, USArrests$Murder, labels=rownames(USArrests))

#Question 2
attach(mtcars)
plot(wt, hp, xlab="Weight", ylab="Horsepower")

median_wt<-median(wt)
grp1<-wt<median_wt
table(grp1)

med_wt1<-median(wt[grp1])
med_wt2<-median(wt[!grp1])

med_hp1<-median(hp[grp1])
med_hp2<-median(hp[!grp1])

abline(v=med_wt1, col="cyan3", lwd=2)
abline(v=med_wt2, col="cyan3", lwd=2)

abline(h=med_hp1, col="orange1", lwd=2)
abline(h=med_hp2, col="orange1", lwd=2)

points(c(med_wt1, med_wt2), c(med_hp1, med_hp2),pch=16, cex=2, col="green4")

rise<-med_hp2-med_hp1
run<-med_wt2-med_wt1
slope<-rise/run

intercept = med_hp1-(slope*med_wt1)
abline(intercept, slope, col="red", lwd=3)

#Question 3
attach(cars)
help(cars)
plot(speed, dist, xlab="Speed", ylab="Stopping Distance")

reg<-lm(dist~speed)
abline(reg, col="deepskyblue4", lwd=2)
reg$coefficients

#Question 4
help(satgpa)
attach(satgpa)

plot(hs_gpa,sat_sum, xlab="HS GPA", ylab="SAT Sum")
line<-lm(sat_sum~hs_gpa)
abline(line,col="royalblue3", lwd=2)
line$coefficients

#SST
SST<-sum((sat_sum-mean(sat_sum))^2)
SST

#SSE
resid<-sat_sum-(66.98845+(11.36317*hs_gpa))
SSE_median<-sum(resid^2)
SSE_median

lm_resid<-sat_sum-(line$coef[1]+(line$coef[2]*hs_gpa))
SSE_lm<-sum(lm_resid^2)
SSE_lm

SSR<- SST-SSE_lm
SSR
line$residuals

median_r<-1-(SSE_median/SST)
lm_r<-1-(SSE_lm/SST)

lm_r
median_r
summary(line)$r.square

#Question 5
help(cars04)
attach(cars04)
carsPlot<-plot(weight, hwy_mpg, xlab = "Highway MPG", ylab="Weight")
carsLine<-lm(hwy_mpg~weight)
abline(carsLine, col="darkred", lwd=2)
carsLine$coefficients

b0=carsLine$coef[1]
b1=carsLine$coef[2]

mpg_3802<-b0+b1*3802
mpg_3802

mpg1700<-b0+b1*1700
mpg1700

b1*100

plot(wt, hp, xlab="Weight", ylab="Horsepower")

#function
median_median <-function(x,y){
  
  median_wt<-median(x)
  grp1<-x<median_wt
  
  med_x1<-median(x[grp1])
  med_x2<-median(x[!grp1])
  
  med_y1<-median(y[grp1])
  med_y2<-median(y[!grp1])
  
  rise<-med_y2-med_y1
  run<-med_x2-med_x1
  slope<-rise/run
  intercept <- med_y1-(slope*med_x1)
  #print(slope)
  #print(intercept)
  return(list(slope=slope, intercept=intercept))
}
#sample
X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
Y <- c(2, 4, 5, 7, 6, 8, 9, 11, 12, 10, 14, 15)
median_median(X, Y)

help(cars)
median_median(x= cars$speed, y=cars$dist)

rows_sample <- sample(1:nrow(cars), replace=TRUE)
median_median(x=cars$speed[rows_sample], y=cars$dist[rows_sample])

rows_sample

slope_grp<-c()
intercept_grp<-c()
for(i in 1:200){
  rows_sample <- sample(1:nrow(cars), replace=TRUE)
  results<-median_median(x=rows_sample, y=cars$dist)
  slope_grp<-c(slope_grp, results$slope)
  intercept_grp<-c(intercept_grp, results$intercept)
}
slope_grp
intercept_grp
hist(slope_grp)
hist(intercept_grp)


slope_grp<-c()
intercept_grp<-c()
for(i in 1:200){
  rows_sample <- sample(1:nrow(cars), replace=TRUE)
  results<-median_median(x=cars$speed[rows_sample], y=cars$dist[rows_sample])
  slope_grp<-c(slope_grp, results$slope)
  intercept_grp<-c(intercept_grp, results$intercept)
}
slope_grp
intercept_grp
hist(slope_grp)
hist(intercept_grp)

## QUESTION 7
lm_reg <- lm(cars$dist~cars$speed)
coef(lm_reg)

rows_sample <- sample(1:nrow(cars), replace=TRUE)
lm_sample<-lm(cars$dist[rows_sample]~cars$speed[rows_sample])
coef(lm_sample)

slope_grp_lm<-c()
intercept_grp_lm<-c()
for(i in 1:200){
  rows_sample <- sample(1:nrow(cars), replace=TRUE)
  lm_result<-lm(cars$dist[rows_sample]~cars$speed[rows_sample])
  slope_grp_lm<-c(slope_grp_lm, lm_result$coefficients[2])
  intercept_grp_lm<-c(intercept_grp_lm, lm_result$coefficients[1])
}
hist(slope_grp_lm, main ="Slope Histogram", xlab="Slope")
hist(intercept_grp_lm, main ="Intercept Histogram", xlab="Intercept")


plot(cars$speed, cars$dist, xlab="Speed", ylab="Stopping Distance")
for(i in 1:200){
  rows_sample <- sample(1:nrow(cars), replace=TRUE)
  lm_result<-lm(cars$dist[rows_sample]~cars$speed[rows_sample])
  slope_grp_lm<-c(slope_grp_lm, lm_result$coefficients[2])
  intercept_grp_lm<-c(intercept_grp_lm, lm_result$coefficients[1])
}

for(i in 1:10){
  abline(a=intercept_grp_lm[i], b= slope_grp_lm[i], col="red3", lwd=1)
}

