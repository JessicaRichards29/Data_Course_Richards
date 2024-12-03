"/Data/non_linear_relationship.csv"
library(tidyverse)

non_linear_dat <- read.csv('non_linear_relationship.csv')

View(non_linear_dat)
#Write the code you would use to model the data found in 
#“/Data/non_linear_relationship.csv” with a linear model
#(there are a few ways of doing this)

non_linear_dat %>% 
  ggplot(aes(x = predictor,
             y = response)) +
  geom_point() +
  geom_smooth(method = "loess")

model <- lm(response ~ poly(predictor, 1), data = non_linear_dat)
summary(model)