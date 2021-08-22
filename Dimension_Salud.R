library(tidyverse)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(plotly)
library(sf)
library(patchwork)
display.brewer.all(type = "qual")
hrbrthemes::import_titillium_web()

# Analisis de la dimensión salud

rm(list = ls())

Salud <- read.csv("IVS.csv")
colnames(Salud)

#Me quedo con variables de interes.
Salud <- Salud %>% select( ,c(2,3,7))

Salud %<>% group_by(comuna) %>% 
            mutate(media = round(mean(ind_salud),4),
                   zona = case_when(comuna %in% c(2,13:15) ~ "Norte",
                                    comuna %in% c(1,4,7:9) ~ "Sur",
                                    comuna %in% c(3,5:6) ~ "Centro",
                                    comuna %in% c(10:12) ~ "Oeste"
                                      ))
                                    
#Grafico
Barras <- Salud %>%
  group_by(comuna) %>% 
  ggplot(aes(x = reorder(as.character(comuna), media),
             y = media,
             fill = zona
             )) +
  geom_bar(stat = "identity",
           position = "identity",
           width = 0.95,
           show.legend = T) +
  geom_text(aes(label= media),
            vjust = -0.25,
            size = 2.8,
            fontface = "italic") +
  labs(title = "Índice de vulnerabilidad de salud por comuna. ",
       subtitle = "CABA, 2019. ",
       x = "Comunas",
       y = "Media",
       caption = "Fuente: elaborado en base a datos de EPH (2019).",
        fill = "Zona") +
  scale_fill_manual(values = c("#219ebc", "#023047", "#ffb703", "#fb8500")) +
  theme_ipsum_tw()+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust= 1,
                                   vjust = 0.2, 
                                   size = 8),
        plot.title = element_text(size=14, face = "bold.italic"),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8))

#Mapa
#Datos
caba<-st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson")
caba <- caba%>%
  rename(comuna=COMUNAS)%>%
  select(comuna, geometry)
caba$comuna<-as.character(caba$comuna)

#Uno datos
Salud$comuna <- as.character(Salud$comuna)
Salud <-left_join(x=caba, y=Salud)
Salud <- Salud %>% mutate(centroide = st_centroid(geometry))

#Mapeo
media_salud<-ggplot() +
              geom_sf(data=Salud, 
              aes(fill=media))+
              scale_fill_viridis_b()
plot_media


(media_salud / Barras)




