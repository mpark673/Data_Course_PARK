# Assignment 9 Mason Park

library(tidyverse)
library(modelr)
library(easystats)

df <- read_csv("../../Data/GradSchool_Admissions.csv")

# Make "admit" binomial for logistic regression
df <- df %>% 
  mutate(admit = case_when(admit == 1 ~ TRUE,
                              TRUE ~ FALSE))
mod1 <- glm(data = df,
            formula = admit ~ gpa + gre + rank,
            family = "binomial")

mod2 <- glm(data = df,
            formula = admit ~ gpa * gre * rank,
            family = "binomial")

mod3 <- glm(data = df,
            formula = admit ~ (gpa + gre) * rank,
            family = "binomial")

mod4 <- glm(data = df,
            formula = admit ~ (gpa * gre) + rank,
            family = "binomial")

compare_performance(mod1,mod2,mod3,mod4)
check_model(mod2)
check_model(mod3)

# This doesn't show much of a difference between the models, but some look a little better than others.
# None of these models have a RMSE in a good range, with the highest being
# 0.442. Mod2 has a higher R2 with better weights, so I'm faced with a choice between mod2 and mod3. Looked into what
# Tjur's R2 is and it is a pseudo R2, meaning it does not represent the proportionate reduction in error as the R2 in 
# linear regression does. Because of this I don't know the effective R2, and I am going to base my choice in model on
# RMSE, or mod3. The check_model doesn't seem to show much of a difference, but colinearity looks a bit better in mod3
# and top end and bottom end residuals look a bit better as well.

pred <- add_predictions(df,mod3, type="response")

g1 <- pred %>% 
  ggplot(aes(x=gpa,y=pred, color = as.factor(rank))) +
  geom_smooth()+
  theme_minimal()

g2 <- pred %>% 
  ggplot(aes(x=gre,y=pred, color = as.factor(rank))) +
  geom_smooth()+
  theme_minimal()
mean(df$gre)

names(df)

mason <- data.frame(gre = 500,gpa = 3.55,rank = 3)

add_predictions(mason,mod3,type="response")
