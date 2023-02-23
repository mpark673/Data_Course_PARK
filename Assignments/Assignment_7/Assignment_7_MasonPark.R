# Assignment 7
library(tidyverse)
library(janitor)
library(skimr)
# Importing the data set
df <- read_csv("./Utah_Religions_by_County.csv") %>% 
  clean_names()

# Cleaning process - clean names as we read it in
  # We need to get all of the "religions" from multiple columns into a single column as they represent a variable
    # pivot_longer allows us to join multiple columns together - creating "religion" and "percent_of_pop" columns

clean <- df %>% 
  pivot_longer(-c(county,pop_2010),
               names_to = "sect",
               values_to = "proportion") # THIS IS THE TIDY VERSION - I used "sect" because I can never remember how to spell "religion"

# This went through many revisions - I tried having "religious" as its own separate variable, but I couldn't get what I wanted out of it 
# So I put it in alongside "non_religious" and it gave better data on the ggplot. 

clean2 <- df %>% 
  pivot_longer(-c(county,pop_2010,religious),
               names_to = "sect",
               values_to = "proportion")

GGally::ggpairs(clean2,cardinality_threshold = 30) # I prefer this graph with total religious% separate as it shows the total religous percentage
# have a slight correlation with total population, however for the ggplot graph including it within "sect" showed a lot better on a graph.

clean %>% 
  ggplot(aes(x=reorder(county,pop_2010),y=proportion,color=sect))+
  facet_wrap(~sect)+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90)) # full zoom the graph for the best view - This graph plots "county" in ascending order of population
# I think this shows visually that there doesn't seem to be any correlation between population and any specific religion or being religious in general.
# However it is more difficult to tell between specific sects.

cor.test(clean$proportion,clean$pop_2010) # this mathematically shows low correlation

clean %>% 
  ggplot(aes(x=proportion,y=sect,color=sect))+
  facet_wrap(~county)+
  geom_point() # I really like this graph as it shows individual comparison between religions within a given county - A correlation I can kind of see is that
# The propotion of lds negatively correlates with non_religious and positively with religious, regardless of county. I don't know how to do this with 
# the correlate functions that I know so I guess this is all I can do for now.
