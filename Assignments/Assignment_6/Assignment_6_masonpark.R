# Assignment 6

# Library and read, this was given

library(tidyverse)
library(gganimate)
df <- read_csv("../../Data/BioLog_Plate_Data.csv")
df
# Cleaning Data into tidy (long) form

df <- df %>% 
  pivot_longer(c(`Hr_24`, `Hr_48`,`Hr_144`), names_to = "Time", values_to = "Absorbance")

# Creating a new column that shows sample type

df$Type <- df$`Sample ID`

df <- df %>% 
  mutate(Type=recode(Type,Clear_Creek="Water",Soil_1="Soil",Soil_2="Soil",Waste_Water="Water"))

# Generate a plot matching the one on the website

df <- df %>% 
  mutate(Time=recode(Time,Hr_24=24,Hr_48=48,Hr_144=144))
df
df %>% 
  filter(Dilution==0.1) %>% 
  ggplot(aes(x=Time,y=Absorbance,color=Type))+
  facet_wrap(~Substrate)+
  geom_smooth(se=F)+
  theme_minimal()



# Generate an animated Plot

p <- df %>% 
  filter(Substrate=="Itaconic Acid") %>%
  group_by(`Sample ID`,Time,Dilution) %>% 
  summarize(Mean_absorbance=mean(Absorbance,na.rm = T)) %>% 
  ggplot(aes(x=Time,y=Mean_absorbance,color=`Sample ID`))+
  facet_wrap(~Dilution)+
  geom_line()

p+transition_reveal(Time)
anim_save("./Assignment6AnimGraph")

