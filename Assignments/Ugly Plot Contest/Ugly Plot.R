# UGLY PLOT CONTEST

library(tidyverse)
library(ggimage)
library(gganimate)
library(magick)
library(palmerpenguins)
df <- read.csv("./Exams/Exam_1/cleaned_covid_data.csv")

df$image = (image = sample("./Images/erm.png", size=33456, replace = T))

penguins$image = (image = sample("./Images/erm.png", size=150, replace = T))

iris %>% 
  ggplot(aes(x=Sepal.Length,y=Petal.Width))+
  geom_point()

p2 <- penguins %>% 
  ggplot(aes(x=body_mass_g,y=bill_length_mm,image=image))+
  geom_image()+
  facet_wrap(~Last_Update)+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold", angle = 180, size = 20,),
        axis.title.y = element_text(face = "bold", angle = 244, size = 15),
        plot.background = element_rect(fill="red"),
        panel.grid = element_line(linewidth = 0.5,color="red"))+
  labs(x="AHHHHHHHHHHHHHHH",
       y="HELP ME")+
  scale_y_log10()+
  coord_polar()

p2

ggbackground(p2,background = "./Images/Oh.png")

