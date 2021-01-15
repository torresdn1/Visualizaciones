library(tidyverse)
library(ggrepel)
library(readxl)
library(wesanderson)
library(spotifyr) 

#Los datos fueron obtenidos de Spotify a través de get_artist_audio_features()
# badbunny <- get_artist_audio_features('Bad Bunny')
# daddyyankee <- get_artist_audio_features('Daddy Yankee')
# jbalvin <- get_artist_audio_features('J Balvin')
# karolg <- get_artist_audio_features('Karol G')
# natti <- get_artist_audio_features('Natti Natasha')
# 
# badbunny <- badbunny %>%  mutate(artista="Bad Bunny")
# daddyyankee<- daddyyankee %>%  mutate(artista="Daddy Yankee")
# jbalvin <- jbalvin %>%  mutate(artista="J Balvin")
# karolg <- karolg %>%  mutate(artista="Karol G")
# natti <- natti %>%  mutate(artista="Natti Natasha")
# reggeaton <- rbind(badbunny, daddyyankee, jbalvin, karolg, natti)
# write_excel_csv(reggeaton, "reggeaton.csv")

#Datos
reggeaton <- read_excel("~/Desktop/Github/Visualizaciones/Reggeaton/Datos/reggeaton_042020.xlsx")

#Identificar outliers
reggeaton <- reggeaton %>%
  filter(album_name!="Energía") %>% 
  group_by(album_name) %>% 
  mutate(outliers= ifelse(quantile(track_popularity,.75)+ 1.5*IQR(track_popularity)< track_popularity|
                            quantile(track_popularity,.25)- 1.5*IQR(track_popularity)> track_popularity, 1, 0),
         maximo= ifelse(outliers==1, 0, 
                        ifelse(max(track_popularity)==track_popularity, 1, 0)))



#Vis
reggeaton %>% 
  ggplot(aes(x=reorder(album_name, album_popularity), y=track_popularity, label= track_name))+
  geom_boxplot(aes(fill=artista), alpha=0.5, outlier.shape = NA)+ 
  geom_jitter(aes(color=artista), size = 5, alpha=0.6, position = position_jitter(seed = 1))+
  geom_text(data= reggeaton %>% filter(outliers==1), size= 5, position = position_jitter(seed = 1))+
  geom_text(data= reggeaton %>% filter(maximo==1), size= 5,  position = position_jitter(seed = 1))+
  scale_y_continuous(breaks = seq(from= 0, to= 100, by= 20), 
                     limits = c(0, 100)) +
  coord_flip()+
  labs(title= "Reggeaton pre-pandemia: Popularidad en Spotify",
       subtitle= "Comparativa entre los álbumes de 5 reggeatonerxs destacados",
       caption = "Nota: Los datos fueron recopilados en Abril de 2020.\nEl grado de popularidad es mayor mientas más cercano esté del 100.\nFuente: Spotify",
       x= "",y= "Popularidad", fill= NULL, color=NULL)+
  scale_color_manual(values = wes_palette(n =5, name = "Darjeeling1", type = "discrete"))+
  scale_fill_manual(values = wes_palette(n =5, name = "Darjeeling1", type = "discrete"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size=35),
        plot.subtitle = element_text(size=32), 
        plot.caption = element_text(face= "italic", size=30),
        text= element_text(family="Times New Roman", size = 32))
  

