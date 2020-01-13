# Data manipulation
library(dplyr)
# Plotting
library(ggplot2)
library(GGally)
# Variable Inflation Factor (VIF)
library(car) 

# Load data:
wrangler <- read.csv("Wrangler.csv") 
View(wrangler) 

#### Linear filtering of time series, by chronological order (year) ####
wrangler.train <- filter(wrangler, Year <= 2015) 
wrangler.test <- filter(wrangler, Year > 2015)
# Confirm proper training
View(wrangler.train)
View(wrangler.test)

ggscatmat(wrangler, columns = 2:8, alpha = 0.8)

#### model based on new variable ####
mod1 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy + 
             CPI.All, data = wrangler.train)
vif(mod1)
summary(mod1)

ggcoef(
  mod2,
  vline_color = "red",
  vline_linetype =  "solid",
  errorbar_color = "purple",
  errorbar_height = .25,
  exclude_intercept = TRUE)

#### model 2 : we remove variable CPI.Energy because high p-value

mod2 <- lm(WranglerSales ~ Unemployment + WranglerQueries +  CPI.All, data = wrangler.train)
vif(mod2)
summary(mod2)

#### model 3 : we remove CPI.all because biggest VIF. The highest is the VIF value
##or a variable the more there are multicollinearity problems linked to this variable

mod3 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy , 
           data = wrangler.train)
vif(mod3)
summary(mod3)


#### Evaluate model2 on testing set ####
# Compute y^hat_i
predictions_WranglerSales <- predict(mod2, newdata=wrangler.test) 
summary(predictions_WranglerSales)

# Sum of Squared Errors from regression model 
SSE_a = sum((wrangler.test$WranglerSales - predictions_WranglerSales)^2)
# Sum of Squared Erros from baseline model 
SST_a = sum((wrangler.test$WranglerSales - mean(wrangler.train$WranglerSales))^2)
# Out of Sample R^2
OSR2_a = 1 - SSE_a/SST_a
OSR2_a

##not a good result because 0,52 << 0,79 (R2), this means overfitting, we try with model 3

#### Evaluate model3 on testing set ####
# Compute y^hat_i
predictions_WranglerSales <- predict(mod3, newdata=wrangler.test) 
summary(predictions_WranglerSales)

# Sum of Squared Errors from regression model 
SSE_a = sum((wrangler.test$WranglerSales - predictions_WranglerSales)^2)
# Sum of Squared Erros from baseline model  
SST_a = sum((wrangler.test$WranglerSales - mean(wrangler.train$WranglerSales))^2)
# Out of Sample R^2
OSR2_a = 1 - SSE_a/SST_a
OSR2_a
##it is better because higher value of OSR2 but there is still overfitting

######Now, we try to improve the model by modelling seasonality 
mod4 <- lm(WranglerSales ~ MonthFactor + Unemployment + WranglerQueries + CPI.Energy + CPI.All, data = wrangler.train)
summary(mod4)
vif(mod4) #high values, lot of multicollinearity. 


##we test model4

# Compute y^hat_i
predictions_WranglerSales <- predict(mod4, newdata=wrangler.test) 
summary(predictions_WranglerSales)

# Sum of Squared Errors from regression model 
SSE_a = sum((wrangler.test$WranglerSales - predictions_WranglerSales)^2)
# Sum of Squared Erros from baseline model  
SST_a = sum((wrangler.test$WranglerSales - mean(wrangler.train$WranglerSales))^2)
# Out of Sample R^2
OSR2_a = 1 - SSE_a/SST_a
OSR2_a
  
##we observe that the R2 is greater. But, some p-values are really high, 
##close to 1 for most of them.   


#new model without CPI.all and without unemployment : we observe a decrease of multicollinearity 
mod6 <- lm(WranglerSales ~ MonthFactor +  WranglerQueries + CPI.Energy , data = wrangler.train)
summary(mod6)
#we test model6 on our testing set

# Compute y^hat_i
predictions_WranglerSales <- predict(mod6, newdata=wrangler.test) 
summary(predictions_WranglerSales)

# Sum of Squared Errors from regression model 
SSE_a = sum((wrangler.test$WranglerSales - predictions_WranglerSales)^2)
# Sum of Squared Erros from baseline model  
SST_a = sum((wrangler.test$WranglerSales - mean(wrangler.train$WranglerSales))^2)
# Out of Sample R^2
OSR2_a = 1 - SSE_a/SST_a
OSR2_a

#we have a better OSR2 when we take into account month factor 

##We create a new model with only monthfactor and cpi.all

mod7 <- lm(WranglerSales ~ MonthFactor + CPI.All , data = wrangler.train)
vif(mod7)
summary(mod7)

predictions_WranglerSales <- predict(mod7, newdata=wrangler.test) 
summary(predictions_WranglerSales)

SSE_a = sum((wrangler.test$WranglerSales - predictions_WranglerSales)^2)
SST_a = sum((wrangler.test$WranglerSales - mean(wrangler.train$WranglerSales))^2)
OSR2_a = 1 - SSE_a/SST_a
OSR2_a

####We add a new feature which corresponds to the average hourly earnings
newfeature <- cbind(wrangler, AHETPIfinal)
nrow(newfeature)

newfeature.train <- filter(newfeature, Year <= 2015) 
newfeature.test <- filter(newfeature, Year > 2015)

mod8 <-lm(WranglerSales ~ MonthFactor + CPI.All + AHETPI , data = newfeature.train)
vif(mod8)
summary(mod8)

predictions_WranglerSales <- predict(mod8, newdata=newfeature.test) 
summary(predictions_WranglerSales)

SSE_a = sum((newfeature.test$WranglerSales - predictions_WranglerSales)^2)
SST_a = sum((newfeature.test$WranglerSales - mean(newfeature.train$WranglerSales))^2)
OSR2_a = 1 - SSE_a/SST_a
OSR2_a  
  
  
  
