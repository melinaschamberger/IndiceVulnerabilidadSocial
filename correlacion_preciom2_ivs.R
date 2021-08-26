Precm2<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Precm2xC.geojson")
palm2<-colorNumeric(palette="Greens", domain=Precm2$US_x_m2)

library(magrittr)
colnames(Precm2)

Precm2 %<>% group_by(COMUNAS.x) %>% 
              summarise(mean(US_x_m2))


cor(ivs_x_comuna$media ~ Precm2$`mean(US_x_m2)`, method = "pearson")

summary(ivs_x_comuna$media)

boxplot(ivs_x_comuna$media, Precm2$`mean(US_x_m2)`)

correlacion <- cor(x =Precm2$`mean(US_x_m2)`, y = ivs_x_comuna$media, method = "pearson")
determinacion <- (correlacion^2)
grado_relacion <- data.frame(correlacion, determinacion)
