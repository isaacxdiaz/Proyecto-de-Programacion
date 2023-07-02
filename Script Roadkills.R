# Establecer la ruta del directorio de trabajo
#rm(list = ls())
ruta = setwd("Your_work_directory")

##### VISUALIZACIÓN DE MAPAS DE ATROPELLOS A NIVEL NACIONAL #####

# Instalar paquetes de ser necesario y cargar las librerías requeridas
#install.packages(c("leaflet", "leaflet.extras", "rnaturalearth", "devtools", "RColorBrewer"))
devtools::install_github("ropensci/rnaturalearthhires")
library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(sp)
library(sf)
library(devtools)
library(rnaturalearthhires)
library(ggplot2)
library(RColorBrewer)
library(carData)
library(car)
library(corrplot)

# Cargar y leer la base de datos
datos_crudos = read.csv("basefinal1.0.csv", header = TRUE)

# Crear dataframe únicamente con los datos de interés para el proyecto
datos <- data.frame(id = datos_crudos$id,
                     lat = datos_crudos$latitude,
                     lon = datos_crudos$longitude,
                     taxon = datos_crudos$iconic_taxon_name,
                     obs = datos_crudos$observed_on)
datos2 = datos

# Eliminar NA 
datos2 <- na.omit(datos)

# Chequear el resumen con descripción breve de cada variable
summary(datos2)

# Definir la proyección de coordenadas para Costa Rica
crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Crear un objeto SpatialPointsDataFrame a partir de las coordenadas en la base de datos
puntos <- SpatialPointsDataFrame(coords = datos2[, c("lon", "lat")], data = datos2, proj4string = CRS(crs))

# Descargar los datos geoespaciales de las provincias de Costa Rica
provincias <- ne_states(country = "Costa Rica", returnclass = "sf")

# Guardar el archivo shapefile de las provincias
st_write(provincias, dsn = paste0(ruta, "/provin.shp"), layer = "provin")

# Leer el archivo shapefile de las provincias de Costa Rica
provincias <- st_read(dsn = paste0(ruta, "/provin.shp"))

# Proyectar las provincias al mismo sistema de coordenadas que los puntos
provincias <- st_transform(provincias, crs)

# Convertir el objeto SpatialPointsDataFrame a sf
puntos_sf <- st_as_sf(puntos)

# Realizar la asignación espacial utilizando st_join
puntos_con_provincia <- st_join(puntos_sf, provincias)

# Obtener la variable provincia asignada
provincia_asignada <- puntos_con_provincia$name

# Agregar la nueva variable "provincia" a tu base de datos
datos2$provincia <- provincia_asignada

# Guardar la base de datos con la nueva variable y limpiar los datos existentes
datos2 <- datos2[datos2$taxon != "", ] 
datos2 <- datos2[datos2$taxon != "Animalia", ] 
datos2 <- datos2[datos2$obs != "", ] 
datos2 <- datos2[complete.cases(datos2$provincia), ]
datos2$taxon[datos2$taxon == "Aves"] <- "Birds"

# Chequear el resumen con descripción breve de cada variable
summary(datos2)

# Crear el archivo .csv final y con los datos pulidos de interés
write.csv(datos2, file = "datos.limpios.csv", row.names = FALSE) 

# Crear primer mapa exploratorio de los datos
m <- leaflet(data = datos) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Latitud:", lat, "<br>Longitud:", lon))

# Visualizar el mapa exploratorio de los datos
m

# Crear un mapa base
mapa <- leaflet(datos2) %>%
  addTiles() %>%
  setView(lng = mean(datos2$lon), lat = mean(datos2$lat), zoom = 6.7)

# Agregar una capa de calor al mapa base
mapa <- mapa %>%
  addHeatmap(lng = ~lon, lat = ~lat, blur = 10, max = 0.5)

# Visualizar el mapa base + la capa de calor
mapa


### Creacción de mapas separados por grupo taxonómico ###
# Obtener los datos de límites administrativos de Costa Rica
costa_rica <- ne_states(country = "Costa Rica", returnclass = "sf")

# Filtrar los datos y excluir la Isla del Coco
costa_rica <- costa_rica[!(costa_rica$name == "Isla del Coco"), ]

# Crear el mapa de muertes con la silueta de Costa Rica sin la Isla del Coco y modificar los márgenes
mapa_atro <- ggplot() +
  geom_sf(data = costa_rica, fill = "white", color = "black") +
  geom_point(data = datos2, aes(x = lon, y = lat, fill = taxon), shape = 21, size = 2) +
  scale_fill_manual(values = c("Amphibia" = "red", "Birds" = "red", "Mammalia" = "red", "Reptilia" = "red")) +
  labs(x = "Longitude", y = "Latitude", title = "Roadkill incidents in Costa Rica from 2013 to 2022 isolated by taxon") +
  theme_classic() +
  facet_wrap(~ taxon, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  coord_sf(xlim = c(-86, -82), ylim = c(8, 11.5))

# Visualizar el mapa de muertes separado
mapa_atro


### Creación de un único mapa con los grupos taxonómicos ###
# Obtener los datos de límites administrativos de Costa Rica
costa_rica <- ne_states(country = "Costa Rica", returnclass = "sf")

# Filtrar los datos y excluir la Isla del Coco
costa_rica <- costa_rica[!(costa_rica$name == "Isla del Coco"), ]

# Definir los colores de la paleta 
colores <- brewer.pal(4, "Spectral")

# Crear el mapa de muertes con la silueta de Costa Rica sin la Isla del Coco y modificar los márgenes
mapa_atro <- ggplot() +
  geom_sf(data = costa_rica, fill = "white", color = "black") +
  geom_point(data = datos2, aes(x = lon, y = lat, fill = taxon), shape = 21, size = 2, alpha = 0.5) +
  scale_fill_manual(values = colores) +
  labs(x = "Longitude", y = "Latitude", title = "Roadkill incidents in Costa Rica from 2013 to 2022", fill = "Taxon") +
  theme_classic() +
  #facet_wrap(~ taxon, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_sf(xlim = c(-86, -82), ylim = c(8, 11.5))

# Visualizar el mapa de muertes junto
mapa_atro


##### ANÁLISIS ESTADÍSTICO #####

# Cargar y leer la base de datos
datos2.conteos <- read.csv("conteos.atro.csv", sep = ";", dec = ".", header = TRUE)

# Verificar que las variables tengan el formato correcto
str(datos2.conteos)

# Leer las variables provincia y taxón como factores
datos2.conteos$provincia = as.factor(datos2.conteos$provincia)
datos2.conteos$taxon = as.factor(datos2.conteos$taxon)
datos2.conteos$mes = as.factor(datos2.conteos$mes)

# Realizar prueba de normalidad residual con un modelo lineal
mod <- lm(cuantos ~ taxon + provincia + mes + anno, data = datos2.conteos)
summary(mod)
res <- residuals(mod)
qqPlot(res)
shapiro.test(res)
# Se rechaza la hipótesis nula de que los residuos se comportan normalmente.

# Crear modelo Poisson con base en los conteos
modelo <- glm(cuantos ~ taxon + provincia + mes + anno, family = poisson, data = datos2.conteos)
summary(modelo)
# Verificar si hay sobredispersión en el modelo
897.22/481 # Sí hay sobredispersión (mayor a 1.2)

# Crear modelo binomial negativo (modelo quassipoisson se descarta)
library(MASS)
modelo.bn <- glm.nb(cuantos ~ taxon + provincia + mes + anno, data = datos2.conteos)

# Chequear salida del modelo en resumen
summary(modelo.bn)

# Realizar un análisis de AIC para determinar cuál componente(s) explica más del modelo
AIC_modelo <- AIC(modelo.bn)
AIC_modelo

mod2 <- glm.nb(cuantos ~ taxon, data = datos2.conteos)
AIC_mod2 <- AIC(mod2)
AIC_mod2

mod3 <- glm.nb(cuantos ~ provincia, data = datos2.conteos)
AIC_mod3 <- AIC(mod3)
AIC_mod3

mod4 <- glm.nb(cuantos ~ mes, data = datos2.conteos)
AIC_mod4 <- AIC(mod4)
AIC_mod4

mod5 <- glm.nb(cuantos ~ anno, data = datos2.conteos)
AIC_mod5 <- AIC(mod5)
AIC_mod5

mod6 <- glm.nb(cuantos ~ taxon + provincia, data = datos2.conteos)
AIC_mod6 <- AIC(mod6)
AIC_mod6

mod7 <- glm.nb(cuantos ~ taxon + provincia + mes, data = datos2.conteos)
AIC_mod7 <- AIC(mod7)
AIC_mod7

# Conclusión: El mejor modelo es el que tiene el AIC más bajo, por lo tanto, el primer modelo que incluye las todas las variables es el que explica de mejor manera la relación y/o efecto sobre la variable respuesta.


citation() # cita R
RStudio.Version() #cita R studio
citation("leaflet")
citation("leaflet.extras")
citation("rnaturalearth")
citation("devtools")
citation("RColorBrewer")
citation("sf")
citation("sp")
citation("ggplot2")
citation("carData")
citation("car")
citation("corrplot")
citation("MASS")
