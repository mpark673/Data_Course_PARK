# Exam 2 Mason Park

library(tidyverse)
library(janitor)
library(skimr)
library(easystats)
library(modelr)

# 1. Read in unicef data

df <- read.csv("unicef-u5mr.csv") %>% 
  janitor::clean_names()

# 2. Tidy the data

names(df) <- 
  names(df) %>% 
  str_remove("u5mr_")

clean <- 
  df %>% 
  pivot_longer(-c(country_name,continent,region,),names_to = "year",values_to = "u5mr")

clean$year <- as.numeric(clean$year)

# 3. First plot

clean %>% 
  ggplot(aes(x=year,y=u5mr,group=country_name)) +
  facet_wrap(~continent)+
  geom_line()
# 4. Save plot
ggsave("PARK_Plot_1.png")

# 5. Second plot

clean %>%
  filter(!is.na(u5mr)) %>% 
  group_by(continent,year) %>%
  summarise_at(vars(u5mr), list(mean_u5mr = mean)) %>% 
  ggplot(aes(x=year,y=mean_u5mr,color=continent,group=continent))+
  geom_path(size=3)+
  labs(x="Year",
       y="Mean_U5MR",
       color="Continent")
# 6. Save plot

ggsave("PARK_Plot_2.png")

# 7. Create 3 models of u5mr

mod1 <- glm(data = clean,
            formula = u5mr ~ year)
mod2 <- glm(data = clean,
            formula = u5mr ~ year + continent)
mod3 <- glm(data = clean,
            formula = u5mr ~ year * continent)

# 8. Compare models

summary(mod1)
summary(mod2)
summary(mod3)
# Not much to go on, these are all off by quite a bit, although mod 3 seems to be slightly better than the others
compare_performance(mod1,mod2,mod3)
# Again not much difference between mod 2 and 3, although 3 appears to be better overall than both mod 1 and 2. However 64%
# isnt much better than the 60%

# 9: Plotting the models

clean %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=year,y=pred,color=continent))+
  facet_wrap(~model) +
  geom_line(size=2) +
  labs(x="Year",
       y="Predicted U5MR",
       color="Continent")

# 10 Bonus

ecuador2020 <- data.frame(country_name = "Ecuador",
                          continent = "Americas",
                          region = "South America",
                          year = 2020)
add_predictions(ecuador2020,mod3) # -10 deaths, im sure that is possible

americas <- clean %>% 
  filter(continent == "Americas")

mod4 <- glm(data=americas,
            formula = u5mr ~ country_name + year)

add_predictions(ecuador2020,mod4) # 14.93849 predicted u5mr
# rouding gives 15 predicted under 5 deaths in 2020. Only 2 off from reality, however I am curious if this application
# of filtering is appropriate for other circumstances. I think the reason this works is because I am training the model using
# data only applicable to the Americas, and finding how different countries and time interact with the u5mr. I guess it makes
# sense, as we are limiting the "demographics" to more similar countries in similar regions allow for a more accurate
# representation of the country, but I could be wrong.