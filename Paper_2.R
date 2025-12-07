install.packages("leaps")
library(leaps)
library(car)
library(lmtest)
library(sandwich)
coffee<-read.csv("arabica_data_cleaned.csv")
#cleaning missing values
coffee_clean<-coffee[coffee$Color != ""&
                       coffee$Color != "None" &
                       coffee$Total.Cup.Points != 0,]
coffee_clean<-na.omit(coffee_clean)
summary(coffee_clean)
attach(coffee_clean4)

#setting up factors
countries <- c("Mexico", "Guatemala", "Colombia", "Brazil", "Taiwan", "Honduras", "Costa Rica",
               "Tanzania", "Uganda", "Kenya","China","Ethiopia","Nicuragua")
methods<-c("Washed / Wet","Natural / Dry","Pulped natural / honey","Semi-washed / Semi-pulped")
coffee_clean4 <- coffee_clean

for (i in countries) {
  coffee_clean4[[paste0(i)]] <- coffee_clean4$Country.of.Origin == i
}
coffee_clean4$Other.Country <- !(coffee_clean4$Country.of.Origin %in% countries)

for (i in methods){
  coffee_clean4[[paste0(i)]]<-coffee_clean4$Processing.Method == i
}
coffee_clean4$Other <- !(coffee_clean4$Processing.Method %in% methods)

head(coffee_clean4[])

####################
##NEW MODEL AND ATTEMPTS, COMBINATION OF EVERYTHING BEFORE
## TASTE FACTORS + COUNTRES + PROCESSING METHODS FACTORZIED
reg1<-lm(Total.Cup.Points~1, data=coffee_clean4)

country_var<-tapply(coffee_clean4$Flavor, coffee_clean4$Country.of.Origin, var)
coffee_clean4$weights<-1/country_var[coffee_clean4$Country.of.Origin]

reg3<-lm(log(Total.Cup.Points) ~ Aftertaste + Clean.Cup + Acidity + 
           Uniformity + Balance + Sweetness + Aroma + Body + `Washed / Wet` + 
           Brazil + Ethiopia + China + `Pulped natural / honey` + Category.Two.Defects + 
           Category.One.Defects, data = coffee_clean4)


reg3_weight<-lm(log(Total.Cup.Points) ~ Aftertaste + Clean.Cup + Acidity + 
                  Uniformity + Balance + Sweetness + Aroma + Body + `Washed / Wet` + 
                  Brazil + Ethiopia + China + `Pulped natural / honey` + Category.Two.Defects + 
                  Category.One.Defects, data = coffee_clean4, weights=coffee_clean4$weights)

summary(reg3)
summary(reg3_weight)
weighted_resids <- weighted.residuals(reg3_weight)
dwtest(reg3)
group4<-reg3_weight$fitted.values>median(reg3_weight$fitted.values)
var.test(resid(reg3_weight)[group4], resid(reg3_weight)[!group4])
crPlots(reg3_weight)

###
plot(reg3$fitted.values, reg3$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Fitted vs. Residuals")
abline(h = 0, col = "red")
# Plot Fitted vs. WEIGHTED Residuals
plot(reg3_weight$fitted.values, resid(reg3_weight),
     xlab = "Fitted Values",
     ylab = "Weighted Residuals",
     main = "Fitted vs. Weighted Residuals")
abline(h = 0, col = "red")
qqnorm(weighted_resids)
qqline(weighted_resids, col="red")


######################################
## WOrking with OUTLIERSSSS
##################################
###cooks distance
cooksDist<-cooks.distance(reg3)
plot(cooksDist)
threshold1<-4/(nrow(coffee_clean4)-length(reg3$coefficients)-1)
abline(h=threshold1)
outliers<-which(cooksDist>threshold1)
coffee_clean4[outliers,]
outliers2<-which(cooksDist<=threshold1)
coffee_clean4[outliers2,]

#####
####dffits
dffits_values <- dffits(reg3_weight)
plot(dffits_values, 
     xlab = "Index", 
     ylab = "DFFITS Values",
     main = "DFFITS Plot")
abline(h = 2 * sqrt(length(reg3_weight$coefficients) / nrow(coffee_clean4)), col = "red", lwd=2)
abline(h = -2 * sqrt(length(reg3_weight$coefficients) / nrow(coffee_clean4)), col = "red", lwd=2)
outliers_dffits <- which(abs(dffits_values) > 2 * sqrt(length(reg3_weight$coefficients) / nrow(coffee_clean4)))
outliers_dffits


#########################################
##Coinfidence INtervals
# For unweighted model
confint(reg3, level = 0.95)

# For weighted model
confint(reg3_weight, level = 0.95)

##############
###MODEL TESTING

reg2<-lm(Total.Cup.Points~Aftertaste + Clean.Cup + Acidity + 
           Uniformity + Balance + Sweetness + Aroma + Body + Moisture+altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
           Quakers+Number.of.Bags+Brazil+Mexico+Colombia+Guatemala+Honduras+`Costa Rica`+Taiwan+
           `Washed / Wet`+`Natural / Dry`+`Pulped natural / honey`+`Semi-washed / Semi-pulped`,
         data=coffee_clean4)
forward1<-step(reg1, scope=list(lower=reg1, upper=reg2), 
               direction="forward", k=2)
forward1

stepwiseAIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
                  direction="both", k=2)
stepwiseAIC

forwardBIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
               direction="forward", k=log(nrow(coffee_clean)))
forwardBIC

stepwiseBIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
                  direction="both", k=log(nrow(coffee_clean)))
