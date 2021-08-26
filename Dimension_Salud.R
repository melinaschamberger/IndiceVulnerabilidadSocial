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
Salud <- read.csv("IVS.csv")
colnames(Salud)

#Me quedo con variables de interes.
Salud <- Salud %>% 
  select( ,c(2,3,7))

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

#Mapa
#Datos
caba<-st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson")
caba <- caba%>%
  rename(comuna=COMUNAS)%>%
  select(comuna, geometry)

#Uno datos
caba$comuna<-as.numeric(caba$comuna)
head(caba)

Salud$comuna <- as.numeric(Salud$comuna)
head(Salud)

Salud <-left_join(x=caba, y=Salud)
Salud <-Salud %>%
              mutate(centroide=st_centroid(geometry))

Salud <- Salud %>% mutate(long = unlist(map(Salud$centroide, 1)),
                            lat = unlist(map(Salud$centroide,2)))

head(Salud)

#Mapeo
mapa_simple <-  Salud %>% st_simplify(dTolerance = 1e-03)



#Creo tema para el mapa
theme_custom_map <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_bw(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      complete = TRUE
    )
}

#mapa

ggplot() +
  geom_sf(data=Salud, aes(fill=media))

# Código

media_salud<-ggplot() +
              geom_sf(data=mapa_simple, aes(fill=media)) +
              theme_custom_map() +
              geom_sf(color = "black", size = 0.1) +
              scale_fill_gradientn (colours = rev(grDevices::heat.colors(10)), name = NULL) +
              geom_text(data = mapa_simple, aes(x = long, y = lat, label = comuna), size = 3) +
              labs(title = "Distribución de hogares según índice de vulnerabilidad en salud",
                   subtitle = "CABA, 2019",
                   caption = "Fuente: elaborado en base a datos de EPH (2019).") +
              theme(title = element_text(size=10, face = "bold"),
                    plot.subtitle = element_text(size = 8), 
                    plot.caption = element_text(size = 7, hjust = 1),
                    plot.caption.position = "panel",
                    legend.position = "bottom")


#Segun zonas
              media_salud +
                facet_wrap("zona", ncol = 2)
              

