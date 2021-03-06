#MotoGP

library(tidyverse)
library(readxl)
library(ggbump)
library(stringr)

motogp <- read_excel("~/Desktop/MotoGP/Data/motogp.xlsx")

motogp<- motogp %>% 
  mutate(label= sapply(str_extract_all(motogp$piloto, "\\b[A-Z]+\\b"), paste, collapse= ' '),
         puntos= paste("(", puntos, ")", sep = ""),
         label= paste(label, puntos, sep ="\n"),
         color= ifelse(piloto=="Valentino ROSSI", "a", 
                       ifelse(piloto=="Marc MARQUEZ", "b", 
                              ifelse(piloto=="Jorge LORENZO", "c", 
                                     ifelse(piloto=="Casey STONER", "d",
                                            ifelse(piloto=="Andrea DOVIZIOSO", "e", 
                                                   ifelse(piloto=="Kenny ROBERTS JR", "f",
                                                          ifelse(piloto=="Nicky HAYDEN", "g",
                                                                 ifelse(piloto=="Joan MIR", "h", "i")))))))),
         color= ifelse(color!= "i", color, 
                       ifelse(moto=="Ducati", "j",
                              ifelse(moto=="Kawasaki", "k", 
                                     ifelse(moto=="Suzuki", "l",
                                            ifelse(moto=="Aprilia", "m", "n"))))))
png("chart.jpg", width = 3500, height = 2800)

motogp %>% 
  ggplot(aes(x=año, y=pos, color= color, group= piloto, label= label)) +
  geom_bump(size = 8, alpha=0.3)+
  geom_bump(size = 8, alpha=1,  data = motogp %>% filter(piloto=="Valentino ROSSI")) +
  geom_bump(size = 8, alpha=1,  data = motogp %>% filter(piloto=="Marc MARQUEZ")) +
  scale_y_reverse()+
  geom_point(aes(x = año), size = 8, alpha=0.3) +
  geom_point(size = 8, alpha=0.5,  data = motogp %>% filter(piloto=="Valentino ROSSI")) +
  geom_point(size = 8, alpha=0.5,  data = motogp %>% filter(piloto=="Marc MARQUEZ")) +
  geom_text(size= 6, nudge_y = .3, colour = "black") +
  labs(title= "Two decades of MotoGP: Standings (2000-2020)",
       subtitle= "Each pilot's position and points per season",
       caption = "Plot: @torres_dn1\nData: www.motogp.com",
       x= "",y= "Rank",color= NULL)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(face = "bold", size = 35),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size=50),
        plot.subtitle = element_text(hjust = 0.5, size=45), 
        plot.caption = element_text(face= "italic", size=35))+
  annotate("text", x=1999, y=.9, label="1º", size= 10)+
  annotate("text", x=1999, y=1.9, label="2º", size= 10)+
  annotate("text", x=1999, y=2.9, label="3º", size= 10)+
  annotate("text", x=1999, y=3.9, label="4º", size= 10)+
  annotate("text", x=1999, y=4.9, label="5º", size= 10)+
  annotate("text", x=1999, y=5.9, label="6º", size= 10)+
  annotate("text", x=1999, y=6.9, label="7º", size= 10)+
    annotate("text", x=1999, y=7.9, label="8º", size= 10)+
    annotate("text", x=1999, y=8.9, label="9º", size= 10)+
    annotate("text", x=1999, y=9.9, label="10º", size= 10)+
    annotate("text", x=1999, y=10.9, label="11º", size= 10)+
    annotate("text", x=1999, y=11.9, label="12º", size= 10)+
    annotate("text", x=1999, y=12.9, label="13º", size= 10)+
    annotate("text", x=1999, y=13.9, label="14º", size= 10)+
    annotate("text", x=1999, y=14.9, label="15º", size= 10)+
    annotate("text", x=1999, y=15.9, label="16º", size= 10)+
    annotate("text", x=1999, y=16.9, label="17º", size= 10)+
    annotate("text", x=1999, y=17.9, label="18º", size= 10)+
    annotate("text", x=1999, y=18.9, label="19º", size= 10)+
    annotate("text", x=1999, y=19.9, label="20º", size= 10)+
    annotate("text", x=1999, y=20.9, label="21º", size= 10)+
    annotate("text", x=1999, y=21.9, label="22º", size= 10)+
    annotate("text", x=1999, y=22.9, label="23º", size= 10)+
    annotate("text", x=1999, y=23.9, label="24º", size= 10)+
    annotate("text", x=1999, y=24.9, label="25º", size= 10)+
    annotate("text", x=1999, y=25.9, label="26º", size= 10)+
    annotate("text", x=1999, y=26.9, label="27º", size= 10)+
    annotate("text", x=1999, y=27.9, label="28º", size= 10)+
    annotate("text", x=1999, y=28.9, label="29º", size= 10)+
    annotate("text", x=1999, y=29.9, label="30º", size= 10)+
    annotate("text", x=1999, y=30.9, label="31º", size= 10)+
    annotate("text", x=1999, y=31.9, label="32º", size= 10)+
  scale_x_continuous(breaks = 2000:2020, position = "top")+
  scale_color_manual(values = c("#5DADE2","#FF7F00", "#3b83bd","#cb3234", "#fe0000",
                                "#D9D280", "#B6EBD9", "#D2EBB6", 
                                "#839192", "#95A5A6", "#AAB7B8", "#BFC9CA", "#D5DBDB"))
dev.off() 
