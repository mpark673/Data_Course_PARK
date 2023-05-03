# Exam 4: Redoing Exam 1

library(tidyverse)
library(janitor)
library(skimr)
library(easystats)

# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

df <- read.csv("./cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)

df1<- grepl("A",df$Province_State)
A_states <- df[df1,]

# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)
A_states$Last_Update <- as.Date(A_states$Last_Update)

A_states %>% 
  ggplot(aes(x = Last_Update,y = Deaths))+
  facet_wrap(~Province_State, scales = "free")+
  geom_point()+
  geom_smooth(se = F,method = loess)+
  labs(x = "Last Update", y = "Deaths")

# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame
# object called state_max_fatality_rate. (20 pts)

state_max_fatality_rate <- df %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio=max(Case_Fatality_Ratio,
                                       na.rm = TRUE)) %>%
  arrange(Province_State)

state_max_fatality_rate <- state_max_fatality_rate %>% 
  arrange(desc(Maximum_Fatality_Ratio))

# V. Use that new data frame from task IV to create another plot. (20 pts)

state_max_fatality_rate$Province_State <- as.factor(state_max_fatality_rate$Province_State)

state_max_fatality_rate$Province_State <- factor(state_max_fatality_rate$Province_State,
                                                 levels = state_max_fatality_rate$Province_State %>% 
                                                   unique())

state_max_fatality_rate %>% 
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.3)) +
  labs(x = "State or Province",
       y = "Maximum Fatality Ratio")

# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

df %>% janitor::clean_names() %>% group_by(last_update) %>% 
  summarise(sum = sum(deaths)) %>% 
  ggplot(aes(x = as.Date(last_update), y = sum)) +
  geom_point() +
  labs(x = "Last Update", y = "Cumulative Deaths (US)")
