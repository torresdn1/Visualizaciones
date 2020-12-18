library(tidyverse)
library(sf)
library(haven)


data <- read_dta("delitos_15_19.dta") #Datos disponibles en: https://www.gob.mx/sesnsp
alcaldias <- st_read("alcaldias.shp", stringsAsFactors = FALSE)

data1<- data %>% filter(entidad=="Ciudad de México", 
                        año==2019, 
                        subtipodedelito=="Robo de vehículo automotor") %>% 
  select(municipio, enero:diciembre)
data1[is.na(data1)]<- 0

data1 <- data1 %>% mutate(anual= enero+febrero+marzo+abril+
                            mayo+junio+julio+agosto+
                            septiembre+octubre+noviembre+diciembre) %>% 
  group_by(municipio) %>% summarise(value= sum(anual))

names(data1)[names(data1)=="municipio"]<- "nomgeo"

#Juntamos datos con shapefile: 
data1 <- alcaldias %>% 
  left_join(data1, by="nomgeo")

#Para agregar labels:
delegaciones <- tribble(
  ~city, ~long, ~lat,
  "Álvaro Obregón", 19.336175562, -99.246819712,
  "Azcapotzalco", 19.4853286147, -99.1821069423,
  "Benito Juárez", 19.3806424162, -99.1611346584,
  "Coyoacán", 19.3266672536, -99.1503763525,
  "Cuajimalpa de Morelos", 19.3246343001, -99.3107285253,
  "Cuauhtémoc", 19.4313734294, -99.1490557562,
  "Gustavo A. Madero",19.5040652077, -99.1158642087,
  "Iztacalco", 19.396911897, -99.094329797,
  "Iztapalapa", 19.3491663204, -99.0567989642,
  "La Magdalena Contreras", 19.2689765031, -99.2684129061,
  "Miguel Hidalgo", 19.4280623649, -99.2045669144,
  "Milpa Alta", 19.1394565999, -99.0510954218,
  "Tláhuac", 	19.2769983772, -99.0028216137,
  "Tlalpan", 19.1983396763, -99.2062207957,
  "Venustiano Carranza", 19.4304954545, -99.0931057959,
  "Xochimilco",	19.2451450458, -99.0903636045
) %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326) 

delegaciones <- delegaciones %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

#MAPA:
  ggplot() + 
    geom_sf(data=data1, aes(fill = value))+
    geom_sf(data = delegaciones) +
    geom_text(data = delegaciones, aes(x = long, y = lat, label = city), size=3, nudge_y = 0.010,  check_overlap = T) +
  labs(title= "¿Qué alcaldía de la CDMX es la más peligrosa?",
       subtitle= "Reportes de robo de autos: 2019",
       caption = "Fuente: SESNSP \nElaborada por: Donovan Torres (@torres_dn1)",
       fill= NULL,
       x="", y="")+
  scale_fill_gradient(low = "white", high = "#31a354", na.value = NA)+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", size=20, family="Times New Roman", hjust = 0.5),
        plot.subtitle = element_text(size=15, family="Times New Roman", hjust = 0.5), 
        plot.caption = element_text(face= "italic", size=12, family="Times New Roman"))


