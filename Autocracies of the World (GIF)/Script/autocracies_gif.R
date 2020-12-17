library(tidyverse)
library(wesanderson)
library(gifski)
library(gganimate)
library(png)
library(readxl)

#Datos disponibles en: https://fsi-live.s3.us-west-1.amazonaws.com/s3fs-public/res/Data_Set.xls
data <- read_excel("~/Downloads/Data_Set.xls")

data1 <- data  %>% 
  select(regime_r, year) %>% mutate(year= as.integer(year)) %>% 
  filter(regime_r!="NA") %>% group_by(regime_r, year) %>% 
  summarise(value= n()) %>% ungroup() %>% 
  group_by(year) %>% arrange(value) %>% 
  mutate(order=1:n(), 
         label= paste(regime_r, value, sep = ": "))

gif <- data1 %>% 
  ggplot(aes(x=order, y= value, fill=regime_r, label=label))+ geom_col()+
  geom_text(size=3, nudge_y = 8, colour = "black")+ coord_flip()+
  labs(title= "El mundo y sus gobiernos: Año {frame_time}",
       subtitle="Número y tipo de regímenes políticos en el mundo",
       caption= "Elaborada por: Donovan Torres (@torres_dn1)\nDatos: Autocracies of the World Dataset", 
       fill= NULL,
       x= "", y="")+
  scale_fill_manual(values = wes_palette(n =5, name = "Moonrise2", type = "continuous"))+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size=20, family="Times New Roman", hjust = 0.5),
        plot.subtitle = element_text(size=15, family="Times New Roman", hjust = 0.5), 
        plot.caption = element_text(face= "italic", size=12, family="Times New Roman"))+
  scale_x_discrete(breaks=data1$order, labels=data1$regime_r)+
  transition_time(data1$year) +
  ease_aes('linear')

animate(gif, nframes=100, duration= 20)
