library(glmnet)
library(MASS)
library(leaps)
library(car)
library(caret)

countries<-read.csv("Clean Country.csv")
countries<-na.omit(countries)
countries<-countries[, -c(1,2)]
                     
#Question 1
countriesMod<-lm(Ladder.score~., data=countries)
x<-model.matrix(countriesMod)
y<-countriesMod$model[,1]

set.seed(364)
step1<-cv.glmnet(x,y,alpha=1)
#Question 2
plot(step1)
plot(step1$glmnet.fit)
#question 3
min<-step1$lambda.min
se<-step1$lambda.1se
min
se

step2<-glmnet(x, y, alpha=1, lambda=se)
lasso.coef<-predict(step2, type="coef")
lasso.coef
summary(lasso.coef)
countries$latin_america<-countries$Regional.indicator=="Latin America and Caribbean"

reg1<-lm(Ladder.score~1, data=countries)
reg2<-lm(Ladder.score~life_exp_rank+obes_rank+unemp_youth+oil_exp_rank+petr_exp_rank+
           elec_export_rank+perc_from_other+infl_rank+gdp_per_cap+unemp_rank+tax_collec_as_per_of_gdp+
           budg_sur_or_def+budge_rank+latin_america+Log.GDP.per.capita+Social.support+Healthy.life.expectancy+
           Freedom.to.make.life.choices+Perceptions.of.corruption, data=countries)
reg2$coefficients
summary(reg2)
#question 4
backwardAIC<-step(reg2, scope=list(lower=reg1, upper=reg2), 
               direction="backward", k=2)
backwardAIC
#question 5
backwardBIC<-step(reg2, scope=list(lower=reg1, upper=reg2), 
              direction="backward", k=log(nrow(countries)))
backwardBIC
#question 6
vif(reg2)
vif(backwardAIC)
vif(backwardBIC)
#question 7
set.seed(123)

trainParams<-trainControl(method="cv", number=10)

reg_model<-train(formula(reg2), data = countries, method = "lm", trControl=trainParams)
backward_aic_model<-train(formula(backwardAIC), data = countries, method = "lm", trControl=trainParams)
backward_bic_model<-train(formula(backwardBIC), data = countries, method = "lm", trControl=trainParams)

reg_model$resample
reg_model$results

backward_aic_model$resample
backward_aic_model$results

backward_bic_model$resample
backward_bic_model$results

set.seed(321)

trainParams2<-trainControl(method="cv", number=10)

reg_model2<-train(formula(reg2), data = countries, method = "lm", trControl=trainParams2)
backward_aic_model2<-train(formula(backwardAIC), data = countries, method = "lm", trControl=trainParams2)
backward_bic_model2<-train(formula(backwardBIC), data = countries, method = "lm", trControl=trainParams2)

reg_model2$resample
reg_model2$results

backward_aic_model2$resample
backward_aic_model2$results

backward_bic_model2$resample
backward_bic_model2$results

set.seed(123)

trainParams3<-trainControl(method="cv", number=20)

reg_model3<-train(formula(reg2), data = countries, method = "lm", trControl=trainParams3)
backward_aic_model3<-train(formula(backwardAIC), data = countries, method = "lm", trControl=trainParams3)
backward_bic_model3<-train(formula(backwardBIC), data = countries, method = "lm", trControl=trainParams3)

reg_model3$resample
reg_model3$results

backward_aic_model3$resample
backward_aic_model3$results

backward_bic_model3$resample
backward_bic_model3$results

#question 10
shapiro.test(reg2$residuals)
plot(reg2$fitted.values, reg2$residuals)
crPlots(reg2)

shapiro.test(backwardAIC$residuals)
plot(backwardAIC$fitted.values, backwardAIC$residuals)
crPlots(backwardAIC)

shapiro.test(backwardBIC$residuals)
plot(backwardBIC$fitted.values, backwardBIC$residuals)
crPlots(backwardBIC)

full<-read.csv("Full Country.csv")
clean <- read.csv("Clean Country.csv")
usa<-subset(full, name=="United States")
usa$latin_america<-usa$Regional.indicator=="Latin America and Caribbean"
full$latin_america<-full$Regional.indicator=="Latin America and Caribbean"

predict(backwardAIC, newdata = usa, interval="prediction", level=0.95)
full <- full[!full$name %in% clean$name, ]

pred<-predict(backwardAIC, newdata = full, interval="prediction", level=0.90)
pred <- as.data.frame(pred)
pred

inside <- full$Ladder.score >= pred$lwr & full$Ladder.score <= pred$upr
sum(inside)                      
full$name[!inside & !is.na(inside)]            

predict(backwardAIC)

