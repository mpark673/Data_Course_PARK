# Package Install and Load

library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# Reading in data

df <- read.csv("../..//Data/mushroom_growth.csv")

df %>% 
  ggplot(aes(x=Temperature, y=GrowthRate))+
  facet_wrap(~Species)+
  geom_point()

df %>% 
  ggplot(aes(x=Temperature, y=GrowthRate, fill=Species))+
  facet_wrap(~Humidity)+
  geom_boxplot()

df %>% 
  ggplot(aes(x=Temperature, y=GrowthRate, fill=Humidity))+
  facet_wrap(~Species)+
  geom_boxplot(alpha=.5)

df %>% 
  ggplot(aes(x=as.factor(Temperature), y=GrowthRate, fill=Humidity))+
  facet_wrap(~Species)+
  geom_boxplot(alpha=.5) # this is the good one

df %>% 
  ggplot(aes(x=Light, y=GrowthRate, color=Humidity))+
  facet_wrap(~Species)+
  geom_point(size=3, alpha=.5)

mod1 <- glm(data = df,
            formula = GrowthRate ~ (Temperature + Light + Nitrogen + Humidity) * Species)

mod2 <- glm(data = df,
            formula = GrowthRate ~ (Temperature * Humidity + Light + Nitrogen) * Species) 

mod3 <- glm(data = df,
            formula = GrowthRate ~ (Temperature * Light * Humidity + Nitrogen) * Species)

mod4 <- glm(data = df, 
            formula = GrowthRate ~ (Temperature * Humidity * Light * Species * Nitrogen))
# Calculate Mean^2 Error

residuals(mod1)^2 %>% mean() %>% sqrt() # 62.64733
residuals(mod2)^2 %>% mean() %>% sqrt() # 56.12451
residuals(mod3)^2 %>% mean() %>% sqrt() # 49.7392
residuals(mod4)^2 %>% mean() %>% sqrt() # 48.2947

summary(mod4)
summary(mod3)

compare_performance(mod1,mod2,mod3,mod4) %>% plot
# I know mod4 technically has a better R2 and RMSE, but I like mod 3 more so im using it

# 6: Adding predictions

predtest <- add_predictions(df,mod3)

# 7: Plotting against real data

ggplot(aes(x=as.factor(Temperature), y=GrowthRate, fill=Humidity))+
  facet_wrap(~Species)+
  geom_boxplot(alpha=.5)

predtest %>% 
  gather_predictions(mod3) %>% 
  ggplot(aes(x=as.factor(Temperature),y=GrowthRate,fill=Humidity)) +
  geom_boxplot(alpha = .5) +
  geom_point(aes(y=pred,color=model)) +
  facet_wrap(~Species)

# writing the code for /Data/non_linear_relationship.csv

df2 <- read.csv("../../Data/non_linear_relationship.csv") %>% 
  filter(!predictor == 0)

mod5 <- glm(data = df2,
    formula = response ~ predictor,
    family = Gamma(link=log))
summary(mod5)
    
summary(mod5,dispersion=1)

df3 <- add_predictions(df2,mod5)

df3 %>% 
  gather_predictions(mod5) %>% 
  ggplot(aes(x=predictor, y=response))+
  geom_point()+
  geom_point(aes(y=pred,color=model))
