#1 loads the “/Data/mushroom_growth.csv” data set
library(tidyverse)
library(easystats)
library(modelr)
dat <- read.csv('mushroom_growth.csv')


#2 creates several plots exploring relationships between the response and predictors

dat %>%
  ggplot(aes(x = Temperature,
             y = GrowthRate,
             color = Humidity)) +
  geom_point() +
  facet_wrap(~Species)

dat %>%
  ggplot(aes(x = Nitrogen,
             y = GrowthRate, 
             color = Humidity)) +
  geom_point() +
  facet_wrap(~Species)

dat %>%
  ggplot(aes(x = Light,
             y = GrowthRate,
             color = Temperature,
             shape = Humidity)) +
  geom_point(position = 'jitter') +
  facet_wrap(~Species)


#3 defines at least 4 models that explain the dependent variable “GrowthRate”
mod1 <- lm(glm(data = dat, formula = GrowthRate ~ Light ))
summary(mod1)
mod2 <- glm(data = dat, formula = GrowthRate ~ Light + Temperature)
summary(mod2)
mod3 <- glm(data = dat, formula = GrowthRate ~ Light * Humidity )
summary(mod3)
mod4 <- glm(data = dat, formula = GrowthRate ~ Temperature * Light * Species)
summary(mod4)
mod5 <- glm(data = dat, formula = GrowthRate ~ Light * Temperature * Humidity * Species)
summary(mod5)
mod6 <- glm(data = dat, formula = GrowthRate ~ Humidity * Nitrogen * Light * Temperature * Species)
summary(mod6)

compare_performance(mod1, mod2, mod3, mod4, mod5, mod6) %>% plot()


#4 calculates the mean sq. error of each model
mse1 <- mean(mod1$residuals^2)
print(mse1)

mse2 <- mean(mod2$residuals^2)
print(mse2)

mse3 <- mean(mod3$residuals^2)
print(mse3)

mse4 <- mean(mod4$residuals^2)
print(mse4)

mse5 <- mean(mod5$residuals^2)
print(mse5)

mse6 <- mean(mod6$residuals^2)
print(mse6)


#5 selects the best model you tried
compare_performance(mod1, mod2, mod3, mod4, mod5, mod6)
compare_performance(mod1, mod2, mod3, mod4, mod5, mod6) %>% plot()
compare_models(mod5, mod6)

#mod5 and mod6 were pretty similar in MSE, R-squared, and AIC values:mod6 had 
#a slightly higher R-squared and a slightly lower MSE while mod5 had slightly 
#lower AIC values. When compare_performance was plotted, mod5 seemed to be
#more well rounded overall so I picked mod5 as my best model. 


#6 adds predictions based on new hypothetical values for the independent variables used in your model
df <- dat %>%
  add_predictions(mod5)
df %>% dplyr::select("GrowthRate", "pred")

Light <- c(0, 10, 20)
Temperature <- c(20, 25)
Humidity <- c('Low', 'High')
Species <- c('P.ostreotus', 'P.cornucopiae')

newdf <- expand.grid(
  Light = Light,
  Temperature = Temperature,
  Humidity = Humidity,
  Species = Species)

pred = predict(mod5, newdata = newdf)

hyp_preds <- data.frame(Light = newdf$Light,
                        Temperature = newdf$Temperature,
                        Humidity = newdf$Humidity,
                        Species = newdf$Species,
                        pred = pred)


df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

fullpreds <- full_join(df,hyp_preds)


#7 plots these predictions alongside the real data
ggplot(fullpreds,aes(x=Temperature,y=pred,color=PredictionType)) +
  geom_point(size = 2) +
  geom_point(aes(y=GrowthRate),alpha = .3, color="Black") +
  theme_minimal()


ggplot(fullpreds, aes(x = Light,
                                 y = pred,
                                 color = PredictionType,
                                 shape = Humidity)) +
    geom_point(size = 3) +
    facet_wrap(~Species, scales = 'free') +
    geom_point(aes(y = GrowthRate), size = 2, alpha = .5, color = "Black")

  
ggplot(fullpreds, aes(x = Light,
                        y = pred,
                        color = Temperature,
                        shape = PredictionType)) +
    geom_point(size = 3) +
    facet_wrap(~Species, scales = 'free') +
    geom_point(aes(y = GrowthRate),size = 2, alpha = .5, color = "Black")

  
#best plot, it shows the most information
  ggplot(fullpreds, aes(x = Light,
                        y = pred,
                        color = Temperature,
                        shape = Humidity)) +
    geom_point(size = 3) +
    facet_wrap(~Species, scales = 'free') +
    geom_point(aes(y = GrowthRate),size = 2, alpha = .5, color = "orange")
  
  