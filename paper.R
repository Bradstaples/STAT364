install.packages("leaps")
library(leaps)
library(car)
library(lmtest)
coffee<-read.csv("arabica_data_cleaned.csv")
#cleaning missing values
coffee_clean<-coffee[coffee$Color != ""&
                      coffee$Color != "None" &
                      coffee$Total.Cup.Points != 0,]
coffee_clean<-na.omit(coffee_clean)
summary(coffee_clean)
attach(coffee_clean)

#setting up regression models
reg1<-lm(Total.Cup.Points~1)
reg2<-lm(Total.Cup.Points~Processing.Method+Color+Moisture+Country.of.Origin+altitude_mean_meters+
           Category.One.Defects+Category.Two.Defects+
           Quakers+Variety+Number.of.Bags)

#aic and bic for forward
forward1<-step(reg1, scope=list(lower=reg1, upper=reg2), 
              direction="forward", k=2)
forward1

forward2<-step(reg1, scope=list(lower=reg1, upper=reg2), 
              direction="forward", k=log(nrow(coffee_clean)))
forward2
#aic and bic for backward
backward1<-step(reg2, scope=list(lower=reg2, upper=reg1), 
              direction="backward", k=2)
backward1

backward2<-step(reg2, scope=list(lower=reg2, upper=reg1), 
               direction="backward", k=log(nrow(coffee_clean)))
backward2
#aic and bic for stepwise
stepwiseAIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
               direction="both", k=2)
stepwiseAIC

stepwiseBIC<-step(reg1, scope=list(lower=reg1, upper=reg2), 
               direction="both", k=log(nrow(coffee_clean)))
stepwiseBIC
#finding the best model and checking values
AIC(forward1)
AIC(forward2)
AIC(backward1)
AIC(backward2)
AIC(stepwiseBIC)
AIC(stepwiseAIC)

summary(forward1)
summary(stepwiseAIC)

#forward, model I am testing to get to work, p values for independence and var.test are very low.
#I feel like this could be because of the farms and countries in the dataset having so much overlap? 
#not sure if I should drop this model, try a different one or find a way to work with the clustering.
plot(forward1$fitted.values, forward1$residuals)
abline(h=0, col=2)
group<-forward1$fitted.values>median(forward1$fitted.values)
var.test(forward1$residuals[group], forward1$residuals[!group])

qqnorm(forward1$residuals, cex=2)
qqline(forward1$residuals, col=2, lwd=3)

dwtest(forward1)
vif(forward1)
plot(forward1)

library(glmnet)
x<-model.matrix(Total.Cup.Points~Processing.Method+Country.of.Origin+
                  Category.One.Defects+Category.Two.Defects+Color,
                data=coffee_clean)
y<-coffee_clean$Total.Cup.Points

set.seed(123)

lasso<-cv.glmnet(x,y,alpha=1)
coef(lasso)

#professors suggestion
keep <- c("Brazil","Mexico","Colombia","Guatemala",
          "Honduras","Costa Rica","Taiwan")

coffee_clean2<-coffee_clean

coffee_clean2$Country.of.Origin[!(coffee_clean2$Country.of.Origin %in% keep)] <- "Other"

coffee_clean2$Country.of.Origin <- factor(coffee_clean2$Country.of.Origin,
                                         levels = c(keep, "Other"))

table(coffee_clean2$Country.of.Origin)

reg3<-lm(Total.Cup.Points~Processing.Method+Color+Moisture+
           altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
           Quakers+Number.of.Bags+Country.of.Origin, data=coffee_clean2)

#lasso based grouping
keep <- c("Colombia","Ethiopia","Kenya","Mexico") 

coffee_clean3<-coffee_clean
coffee_clean3$Country.of.Origin[!(coffee_clean3$Country.of.Origin %in% keep)] <- "Other"

coffee_clean3$Country.of.Origin <- factor(coffee_clean3$Country.of.Origin,
                                         levels = c(keep, "Other"))

table(coffee_clean3$Country.of.Origin)

reg3<-lm(Total.Cup.Points~Processing.Method+Color+Moisture+
           altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
           Quakers+Number.of.Bags+Country.of.Origin, data=coffee_clean3)
################################################################################
dwtest(reg3)
summary(reg3)
plot(reg3$fitted.values, reg3$residuals)
abline(h=0, col=2)
group<-reg3$fitted.values>median(reg3$fitted.values)
var.test(reg3$residuals[group], reg3$residuals[!group])
qqnorm(reg3$residuals, cex=2)
qqline(reg3$residuals, col=2, lwd=3)
vif(reg3)
plot(reg3)
dwtest(reg3)
################################################################################

#professor suggestion
countries <- c("Brazil","Mexico","Colombia","Guatemala",
               "Honduras","Costa Rica","Taiwan")
coffee_clean4 <- coffee_clean

for (i in countries) {
  coffee_clean4[[paste0(i)]] <- coffee_clean4$Country.of.Origin == i
}


coffee_clean4$Other <- !(coffee_clean4$Country.of.Origin %in% countries)

head(coffee_clean4[])

## lasso regression, try regions 
coffee_clean5 <- coffee_clean
countries <- c("Colombia","Ethiopia","Kenya","Mexico")

for (i in countries) {
  coffee_clean5[[paste0(i)]] <- coffee_clean5$Country.of.Origin == i
}

coffee_clean5$Other <- !(coffee_clean5$Country.of.Origin %in% countries)

head(coffee_clean5[])
reg5<-lm(Total.Cup.Points~Processing.Method+Color+Moisture+
           altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
           Quakers+Number.of.Bags+Colombia+Ethiopia+Kenya+Mexico+Other,
         data=coffee_clean5)
dwtest(reg5)
#lasso with new grouping
library(glmnet)
x<-model.matrix(Total.Cup.Points~Processing.Method+Color+Moisture+
                  altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
                  Quakers+Number.of.Bags+Brazil+Mexico+Colombia+Guatemala+Honduras+`Costa Rica`+Taiwan+Other,
                data=coffee_clean4)

y<-coffee_clean4$Total.Cup.Points

set.seed(123)

lasso<-cv.glmnet(x,y,alpha=1)
coef(lasso)
reg4<-lm(Total.Cup.Points~Processing.Method+Color+Moisture+
           altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
           Quakers+Number.of.Bags+Brazil+Mexico+Colombia+Guatemala+Honduras+`Costa Rica`+Taiwan+Other,
         data=coffee_clean4)
dwtest(reg4)
summary(reg4)
plot(reg4$fitted.values, reg4$residuals)
abline(h=0, col=2)
group<-reg4$fitted.values>median(reg4$fitted.values)
var.test(reg4$residuals[group], reg4$residuals[!group])
qqnorm(reg4$residuals, cex=2)
qqline(reg4$residuals, col=2, lwd=3)
vif(reg4)
plot(reg4)
dwtest(reg4)
plot(reg4$fitted.values, reg4$residuals)
abline(h=0, col=2)
