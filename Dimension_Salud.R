library(tidyverse)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(sf)
library(patchwork)


# Analisis de la dimensión salud
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
  labs(x = "Comunas",
       y = "Media",
        fill = "Zona") +
  scale_fill_manual(values = c("#219ebc", "#023047", "#ffb703", "#fb8500")) +
  theme_ipsum_tw()+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust= 1,
                                   vjust = 0.2, 
                                   size = 8),
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

Salud <- Salud %>% mutate(long = unlist(map(Salud$centroide, 1)),
                            lat = unlist(map(Salud$centroide,2)))

head(Salud)

#Mapeo
media_salud<-ggplot() +
              geom_sf(data=Salud, aes(fill=zona)) +
              #scale_fill_viridis(option = "C", alpha = .4) +
              scale_fill_manual(values = c("#219ebc", "#023047", "#ffb703", "#fb8500")) +
              geom_text(data =Salud, aes(x = long, y = lat, label = comuna)) +
              geom_sf_label(data = Salud, aes(label = media),
                            size = 3,
                            vjust = 0.8) +
              theme(axis.text.x = element_text(angle = 90, 
                                   hjust= 1,
                                   vjust = 0.2, 
                                   size = 8),
              axis.text.y = element_text(size = 8),
              legend.position = 'none')


graficos_combinados <- (media_salud | Barras) +
                        plot_annotation(title = "Índice de vulnerabilidad de salud por comuna. ",
                                        subtitle = "CABA, 2019. ",
                                        caption = "Fuente: elaborado en base a datos de EPH (2019).") +
                        theme(plot.title = element_text(size=14, face = "bold.italic"),
                              plot.subtitle = element_text(size = 14), 
                              plot.caption = element_text(size = 14)) +
                        theme_ipsum_tw()
                                        