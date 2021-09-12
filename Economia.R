library(tidyverse)
#library(viridis)
library(hrbrthemes)
#library(RColorBrewer)
library(sf)
library(patchwork)
library(ggplot2)
library(grDevices)
#Apagar geometria de los datos
sf::sf_use_s2(FALSE)

# Analisis de la dimensión salud
Economia <- read.csv("IVS.csv")
colnames(Economia)

#Me quedo con variables de interes.
Economia <- Economia %>% 
  select( ,c(2,3,5:6))

Economia %<>% group_by(comuna) %>% 
  mutate(media_depen = round(mean(ind_depend),4),
         media_ingre = round(mean(ind_ingresos),4),
         media_doble = (media_depen+media_ingre)/2,
         zona = case_when(comuna %in% c(2,13:15) ~ "Norte",
                          comuna %in% c(1,4,7:9) ~ "Sur",
                          comuna %in% c(3,5:6) ~ "Centro",
                          comuna %in% c(10:12) ~ "Oeste"
         ))

options(scipen = 999)
#Grafico
Barras <- Economia %>%
  group_by(comuna) %>% 
  ggplot(aes(x = reorder(as.character(comuna), media_doble),
             y = media_doble,
             fill = media_doble
  )) +
  geom_bar(stat = "identity",
           position = "identity",
           width = 0.95,
           show.legend = T) +
  geom_text(aes(label= media_depen),
            vjust = -0.25,
            size = 2.5,
            face = "italic") +
  labs(x = "Comunas",
       y = " ",
       fill = "Zona",
       title = "Distribución de hogares con vulnerabilidad en salud",
       caption = "Fuente: elaborado en base a datos de EPH (2019). ",
       subtitle = "CABA, 2019") +
  scale_fill_manual(values = c("#219ebc", "#023047", "#ffb703", "#fb8500")) +
  theme_ipsum_tw()+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust= 1,
                                   vjust = 0.2, 
                                   size = 8,
                                   face = "italic"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size =10, face = "italic"),
        legend.title = element_text(size = 8, face = "italic"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom", 
        legend.box = "horizontal", 
        legend.text = element_text(face = "italic"),
        plot.caption = element_text(size = 7, hjust = 1))
