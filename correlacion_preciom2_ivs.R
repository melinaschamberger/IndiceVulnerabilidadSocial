Precm2<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Precm2xC.geojson")
palm2<-colorNumeric(palette="Greens", domain=Precm2$US_x_m2)

library(magrittr)
library(ggdist)

##Correlación precio m2 y índice total
colnames(Precm2)

Precm2 %<>% group_by(COMUNAS.x) %>% 
              summarise(mean(US_x_m2))


cor(ivs_x_comuna$media ~ Precm2$`mean(US_x_m2)`, method = "pearson")

summary(ivs_x_comuna$media)

boxplot(ivs_x_comuna$media, Precm2$`mean(US_x_m2)`)

correlacion <- cor(x =Precm2$`mean(US_x_m2)`, y = ivs_x_comuna$media, method = "pearson")
determinacion <- (correlacion^2)
grado_relacion <- data.frame(correlacion, determinacion)


##Veo distribución de cada columna

colnames(Construccion_indice)
str(Construccion_indice)

p3 <- Construccion_indice %>% 
        ggplot(aes(x = id, y = hac, fill = comuna)) +
  stat_halfeye()
  
boxplot(Construccion_indice$hac, Construccion_indice$ind_dominio, Construccion_indice$ind_depend, 
        Construccion_indice$ind_ingresos, Construccion_indice$ind_salud, Construccion_indice$ind_edu, 
        names = c("Ha", "Do", "De", "In", "Sal", "Edu"))
  
#Chequeo educación
Construccion_indice %>% 
  group_by(comuna) %>% 
  rstatix::get_summary_stats(ind_edu, type  = "mean_sd")

data <- Construccion_indice$ind_edu
mean(data, na.rm = T)
sd(data, na.rm = T)
limits_mean <- c(mean(data, na.rm =  T) - 3 * sd(data, na.rm = T),
                 mean(data, na.rm = T) + 3 * sd(data, na.rm = T))
limits_mean
max(data, na.rm = T)

p <- as.data.frame(data)
str(p)
p %>% filter(data >= 0.19)  

install.packages("Routliers")
library(Routliers)
outliers <- outliers_mad(data)
plot_outliers_mad(outliers, x = data)


#Pruebo indice sin edu
ivs2<-Construccion_indice%>%mutate(ind_total=sum(c(ind_dominio,ind_depend,ind_ingresos,
                                                  ind_salud)))

ivs_x_comuna2<-ivs2%>%group_by(comuna)%>%
  summarise(media=mean(ind_total, na.rm = TRUE),
            varianza=var(ind_total, na.rm = TRUE),
            desvio=sd(ind_total, na.rm = TRUE),
            mediana=median(ind_total, na.rm=TRUE))


correlacion <- cor(x =Precm2$`mean(US_x_m2)`, y = ivs_x_comuna2$media, method = "pearson")
determinacion <- (correlacion^2)
grado_relacion <- data.frame(correlacion, determinacion)


columna1 <- ivs_x_comuna2$media
columna1 %<>% as.data.frame(columna1) 
names(columna1) <- c("IVS")


columna2 <- Precm2$`mean(US_x_m2)`
columna2 %<>% as.data.frame(columna2) 
names(columna2) <- c("Precio")

total <- cbind(columna1, columna2)

outliers_multi <- outliers_mcd(total)
plot_outliers_mcd(outliers_multi, x = cbind(columna1, columna2))

ggplot(data = total, aes(x = Precio, y = IVS)) + 
  geom_jitter( size = 1, alpha = 0.7) +
  geom_smooth(se = FALSE,
                         method = lm)

