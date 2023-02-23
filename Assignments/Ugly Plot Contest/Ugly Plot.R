# UGLY PLOT CONTEST

library(tidyverse)
library(ggimage)
library(palmerpenguins)

penguins$image = (image = sample("./Images/uhoh.png", size=344, replace = T))

p2 <- penguins %>% 
  ggplot(aes(x=body_mass_g,y=bill_length_mm,image=image))+
  geom_image()+
  theme_minimal()+
  theme(title = element_text(color = "green", face = "bold", angle = 3, size = 25),
        axis.title.x = element_text(color = "red", face = "bold", angle = 22, size = 20,),
        axis.title.y = element_text(color = "yellow", face = "bold", angle = 253, size = 15),
        axis.text.x = element_text(color = "blue", face = "italic", angle = 167, size = 25),
        axis.text.y = element_text(color = "orange", face = "italic", angle = 134, size = 40),
        panel.grid = element_line(linetype = 0))+
  labs(title="MY EYES",
       x="AHHHHHHHHHHHHHHH",
       y="HELP ME")+
  scale_y_log10()+
  coord_cartesian(xlim = c(3000,4000),ylim = c(35,45))

ggbackground(p2,background = "./Images/erm.png")

ggsave("./Images/I_am_ashamed.png", width = 6, height = 6, dpi = 600)

