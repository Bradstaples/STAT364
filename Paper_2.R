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

#setting up factors
countries <- c("Brazil","Mexico","Colombia","Guatemala",
               "Honduras","Costa Rica","Taiwan")
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
#grouping with WLS

country_var<-tapply(coffee_clean4$Total.Cup.Points, coffee_clean4$Country.of.Origin, var)
coffee_clean4$weights<-1/country_var[coffee_clean4$Country.of.Origin]
reg_wls<-lm(Total.Cup.Points~Color+Moisture+
              altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
              Quakers+Number.of.Bags+Brazil+Mexico+Colombia+Guatemala+Honduras+`Costa Rica`+Taiwan+Other.Country+
              `Washed / Wet`+`Natural / Dry`+`Pulped natural / honey`+`Semi-washed / Semi-pulped`+Other,
            data=coffee_clean4, weights=coffee_clean4$weights)
dwtest(reg_wls)
group3<-reg_wls$fitted.values>median(reg_wls$fitted.values)
var.test(reg_wls$residuals[group3], reg_wls$residuals[!group3])
summary(reg_wls)
vif(reg_wls)
plot(reg_wls$fitted.values, reg_wls$residuals)
#lasso with new grouping and weights
library(glmnet)
x<-model.matrix(Total.Cup.Points~Color+Moisture+
                  altitude_mean_meters+Category.One.Defects+Category.Two.Defects+
                  Quakers+Number.of.Bags+Brazil+Mexico+Colombia+Guatemala+Honduras+`Costa Rica`+Taiwan+Other.Country+
                  `Washed / Wet`+`Natural / Dry`+`Pulped natural / honey`+`Semi-washed / Semi-pulped`+Other,
                data=coffee_clean4)
y<-coffee_clean4$Total.Cup.Points
set.seed(123)
lasso<-cv.glmnet(x,y,alpha=1, weights=coffee_clean4$weights)
coef(lasso)
