install.packages("leaps")
library(leaps)
library(car)
library(caret)
library(lmtest)
library(glmnet)
coffee<-read.csv("arabica_data_cleaned.csv")
#cleaning missing values
coffee_clean<-coffee[coffee$Total.Cup.Points != 0 &coffee$Moisture != 0,]
coffee_clean<-na.omit(coffee_clean)
summary(coffee_clean)
attach(coffee_clean)

#setting up factors
countries <- c("Mexico", "Guatemala", "Colombia", "Brazil", "Taiwan", "Honduras", "Costa Rica",
               "Tanzania", "Uganda", "Kenya","China","Ethiopia","Nicuragua")
methods<-c("Washed / Wet","Natural / Dry","Pulped natural / honey","Semi-washed / Semi-pulped")
color<-c("Green", "Blue-Green", "Bluish-Green")

for (i in countries) {
  coffee_clean[[paste0(i)]] <- coffee_clean$Country.of.Origin == i
}
coffee_clean$Other.Country <- !(coffee_clean$Country.of.Origin %in% countries)

for (i in methods){
  coffee_clean[[paste0(i)]]<-coffee_clean$Processing.Method == i
}
coffee_clean$Other <- !(coffee_clean$Processing.Method %in% methods)

for (i in color){
  coffee_clean[[paste0(i)]]<-coffee_clean$Color == i
}
coffee_clean$Other.Color <- !(coffee_clean$Color %in% color)
head(coffee_clean[])

####################
##NEW MODEL AND ATTEMPTS, COMBINATION OF EVERYTHING BEFORE
## TASTE FACTORS + COUNTRES + PROCESSING METHODS FACTORZIED

reg3<-lm(Total.Cup.Points ~ Aftertaste + Clean.Cup + Acidity + 
           Sweetness + Balance + I(Uniformity^2) + Aroma + Body + 
           `Washed / Wet` + Moisture + Number.of.Bags+ Taiwan  + Kenya + Ethiopia + Green+
           Category.One.Defects +Category.Two.Defects , data = coffee_clean)

summary(reg3)
dwtest(reg3)

group1<-reg3$fitted.values>median(reg3$fitted.values)
var.test(reg3$residuals[group1], reg3$residuals[!group1])
                                              
vif(reg3)
AIC(reg3)
crPlots(reg3)


###
plot(reg3$fitted.values, reg3$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Fitted vs. Residuals")
abline(h = 0, col = "red", lwd=3)
qqnorm(reg3$residuals)
qqline(reg3$residuals, col="red", lwd=2)


######################################
## WOrking with OUTLIERSSSS
##################################
###cooks distance
cooksDist<-cooks.distance(reg3)
plot(cooksDist)
threshold1<-4/(nrow(coffee_clean)-length(reg3$coefficients)-1)
abline(h=threshold1, col="red", lwd=2)

outliers<-which(abs(cooksDist)>threshold1)
coffee_clean[outliers,]
outliers2<-which(cooksDist<=threshold1)
coffee_clean[outliers2,]

outlier_info <- data.frame(
  Row_Number = outliers,
  Country_of_Origin = coffee_clean[outliers, c("Country.of.Origin", "Total.Cup.Points", "Acidity")]
)

print(outlier_info)
#####
####dffits
dffits_values <- dffits(reg3)
plot(dffits_values, 
     xlab = "Index", 
     ylab = "DFFITS Values",
     main = "DFFITS Plot")
abline(h = 2 * sqrt(length(reg3$coefficients) / nrow(coffee_clean)), col = "red", lwd=2)
abline(h = -2 * sqrt(length(reg3$coefficients) / nrow(coffee_clean)), col = "red", lwd=2)
outliers_dffits <- which(abs(dffits_values) > 2 * sqrt(length(reg3$coefficients) / nrow(coffee_clean)))
outliers_dffits


########DFbetas
dfbetas_values <- dfbetas(reg3)
dfbetas_threshold <- 2 / sqrt(nrow(coffee_clean))
influential_points <- which(apply(abs(dfbetas_values), 1, max) > dfbetas_threshold)
length(influential_points)  # How many points are influential
coffee_clean[influential_points,] 
##################################################################################
###k-fold Cross Validation

###10 fold
set.seed(123)
trainParams<-trainControl(method="cv", number=10)

reg_model<-train(formula(reg3), data = coffee_clean, method = "lm", trControl=trainParams)
reg_model$resample
reg_model$results


###20 fold
set.seed(123)
trainParams2<-trainControl(method="cv", number=20)

reg_model2<-train(formula(reg3), data = coffee_clean, method = "lm", trControl=trainParams2)
reg_model2$resample
reg_model2$results



#########################################
##Coinfidence INtervals
# For unweighted model
confidence<-confint(reg3, level = 0.95)
confidence

plot(confidence,
     main = "95% Confidence Intervals for Coefficients",
     xlab = "Coefficient Estimates",
     ylab = "Predictor Variables",
     col = "blue",
     pch = 19)


##############################################################################################################
#####CUPPA PLOTS FOR THE MIND

hist(coffee_clean$Total.Cup.Points,
     breaks = 25,                
     main = "Distribution of Total Cup Points",
     xlab = "Total Cup Points",
     col = "darkred")
# Count of coffees per country
country_counts <- table(coffee_clean$Country.of.Origin)

# Bar plot
barplot(country_counts,
        main = "Number of Coffee Samples per Country",
        xlab = "Country",
        ylab = "Count",
        las = 2,                 # makes labels vertical
        col = "orange")

# Boxplot of Total Cup Points by Processing Method
boxplot(Total.Cup.Points ~ Processing.Method, data = coffee_clean,
        main = "Total Cup Points by Processing Method",
        xlab = "Processing Method",
        ylab = "Total Cup Points",
        col = "lightblue",
        las = 2)
##Prediction Accuracy and intervals
coffee_clean$predicted_points <- predict(reg3, newdata = coffee_clean)
top_actual <- coffee_clean[order(-coffee_clean$Total.Cup.Points), ][1:10, ]
top_actual

top_predicted <- coffee_clean[order(-coffee_clean$predicted_points), ][1:10, ]
top_predicted

pred_vs_actual <- coffee_clean[, c("Country.of.Origin", "Total.Cup.Points", "predicted_points")]
pred_vs_actual[1:10,]

head(pred_vs_actual, 10)
plot(coffee_clean$Total.Cup.Points, coffee_clean$predicted_points,
     xlab = "Actual Total Cup Points",
     ylab = "Predicted Total Cup Points",
     main = "Predicted vs Actual Total Cup Points")
abline(0, 1, col = "red", lwd = 1)  # 45-degree line
##############
###MODEL TESTING
reg1<-lm(Total.Cup.Points~1, data=coffee_clean)
reg2<-lm(Total.Cup.Points~Aftertaste + Clean.Cup + Acidity  +
           Uniformity + Balance + Sweetness + Aroma + Body + Moisture+ altitude_mean_meters+
           altitude_low_meters+ Category.One.Defects+ Category.Two.Defects+
           Quakers+ Number.of.Bags+ Brazil+ Mexico+ Colombia+ Guatemala+ Honduras+`Costa Rica`+
           Taiwan+ Tanzania+ Uganda+ Kenya+ China+ Ethiopia+ Nicuragua+ Green+ `Blue-Green`+ `Bluish-Green`+
           `Washed / Wet`+`Natural / Dry`+`Pulped natural / honey`+`Semi-washed / Semi-pulped`,
         data=coffee_clean)
forward1<-step(reg1, scope=list(lower=reg1, upper=reg2), 
               direction="forward", k=2)
forward1
summary(forward1)

stepwiseAIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
                  direction="both", k=2)
stepwiseAIC
summary(stepwiseAIC)

backward1<-step(reg2, scope=list(lower=reg2, upper=reg1), 
                direction="backward", k=2)
backward1
summary(backward1)

AIC(forward1)
AIC(stepwiseAIC)
AIC(backward1)
top_predicted[, c("Country.of.Origin",
                  "Total.Cup.Points",
                  "predicted_points")]




#########################################
####weighted model, no longer needed 

country_var<-tapply(coffee_clean$Flavor, coffee_clean$Country.of.Origin, var)
coffee_clean$weights<-1/country_var[coffee_clean$Country.of.Origin]
reg3_weight<-lm(Total.Cup.Points ~ Aftertaste + Clean.Cup + Acidity + 
                  Sweetness + Balance + Uniformity + Aroma + Body  + 
                  `Washed / Wet` + Moisture + Number.of.Bags+ Taiwan + Kenya + Ethiopia + Green+
                  Category.One.Defects+Category.Two.Defects , data = coffee_clean, weights=coffee_clean$weights)
summary(reg3_weight)
vif(reg3_weight)
AIC(reg3_weight)
crPlots(reg3_weight)
group2<-reg3_weight$fitted.values>median(reg3_weight$fitted.values)
var.test(resid(reg3_weight)[group2], resid(reg3_weight)[!group2])


##########idk if i should use
#########################################
confidenceUnweighted <- predict(reg3, interval = "confidence", level = 0.95)
confidenceWeighted <- predict(reg3_weight, interval = "confidence", level = 0.95)

predictionUnweighted <- predict(reg3, interval = "prediction", level = 0.95)
predictionWeighted <- predict(reg3_weight, interval = "prediction", level = 0.95)

true<-coffee_clean$Total.Cup.Points
trueWeighted<-reg3_weight$model$Total.Cup.Points

delta <- 1

accuracyUnweighted <- mean(abs(predictionUnweighted[, "fit"] - true)<=delta)
accuracyUnweighted

accuracyWeighted <- mean(abs(predictionWeighted[, "fit"] - trueWeighted)<=delta)
accuracyWeighted



