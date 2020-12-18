library(tidyverse) 
library(highcharter) 
library(lubridate)
library(plotly)

#Los datos están disponibles aquí: https://datos.cdmx.gob.mx/explore/dataset/incidentes-viales-c5/
data_C5 <- read.csv("incidentes-viales-c5.csv") 

#Nota: La base tiene errores en la codificación de las fechas (año 2019 está escrito como 19 y no 2019)
#Los corregiré antes: 

data_C5<- data_C5 %>% separate(fecha_cierre, c("d", "m", "y"), sep = "/") %>% 
  mutate(y= ifelse(y=="19", "2019", y),
         Date = as.Date(paste(d,m,y, sep = "/"), format ="%d/%m/%Y"))

data1 <- data_C5 %>% 
  select(incidente_c4, Date, delegacion_inicio) %>% 
  filter(!is.na(delegacion_inicio), 
         delegacion_inicio!="")

#Incidentes viales por mes y por delegación
data1 <- data1 %>% 
  group_by(month=floor_date(Date, "month"), delegacion_inicio) %>%
  summarize(value=n()) %>% ungroup() 


#######(Highcharter)########
data1 %>%
  hchart(type = "heatmap", name= "No.Incidentes",
         hcaes(x = month, y = reorder(delegacion_inicio,value), value = value)) %>% 
  hc_title(text = "Incidentes viales reportados por el C5 de la Ciudad de México") %>% 
  hc_subtitle(text= "Número de incidentes acumulados por mes") %>% 
  hc_xAxis(title = list(text = "<b>Fecha/Mes</b>"), type = "datetime", 
           dateTimeLabelFormats = list(day = '%H:%M')) %>% 
  hc_yAxis(title = list(text = ""))


######ggplotly#######

heatmap<- data1 %>% 
  mutate(Mes= month, Delegación= reorder(delegacion_inicio,value), Incidentes= value) %>% 
ggplot(aes(x =Mes, y = Delegación, fill= Incidentes)) + geom_tile()+
  labs(title="Incidentes viales reportados por el C5 de la Ciudad de México",
       subtitle="Número de incidentes acumulados por mes",
       caption= "Elaborada por: Donovan Torres (@torres_dn1)\nDatos: C5-Gob-CDMX", 
       fill= NULL)+
  scale_fill_gradient(low = "white", high = "#25425b")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  xlab("") + ylab("")+
  theme(panel.background = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold", size=20, family="Times New Roman", hjust = 0.5),
        plot.subtitle = element_text(size=15, family="Times New Roman", hjust = 0.5), 
        plot.caption = element_text(face= "italic", size=12, family="Times New Roman"))


ggplotly(heatmap) %>% 
  layout(title = list(text = paste0('Incidentes viales reportados por el C5 de la Ciudad de México',
                                    '<br>',
                                    '<sup>',
                                    'Elaborada por: Donovan Torres (@torres_dn1). Datos: C5-Gob-CDMX')))     
rm(a)
