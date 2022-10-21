# Clase 5. 22/10/2022
# Data Viz II: visualización de mapas

### En la última clase del módulo 1 vimos algunas técnicas de visualización de datos categóricos y numéricos, 
### hoy vamos a profundizar en técnicas de visualización de datos georreferenciados.


# 0. Librerías ####
library(tidyverse)
#install.packages("sf")
library(sf)
#install.packages("spatialEco")
library(spatialEco)
# install.packages("ggmap")
library(ggmap)
#install.packages("leaflet")
library(leaflet)


# Datos vectoriales

# 1. Lectura de datos y tipos de archivos (sf::st_read()) ####

## 1.1. Geojson <- .geosjon ####
## Hospitales y comunas de CABA. 

hospitales <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-salud/hospitales/hospitales.geojson")
comunas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Comunas.geojson")

class(hospitales)
class(comunas)

## 1.2. Shapefile <- .shp ####
## Recorridos de líneas de colectivo de CABA (https://data.buenosaires.gob.ar/dataset/colectivos-recorridos)

getwd()
colectivos <- st_read("C:/Users/Melina/OneDrive/Documentos/Proyectos R/Curso-R-m2/data/clase5/recorridos/Lineas_CABA_BADATA.shp")

class(colectivos)

## 1.3. Csv con atributos geo <- .csv ####
## Postas de vacunación COVID-19

clase <- "clase5"
postas_covid <- read_csv(paste0("./data/",clase,"/postas_vacunacion_covid.csv"))
class(postas_covid)

# setear la variable geo (sf::st_as_sfc())
postas_covid$WKT <- st_as_sfc(postas_covid$WKT)

# convertir el df a uno de tipo geo (sf::st_as_sf())
postas_covid <- st_as_sf(postas_covid)
class(postas_covid)

# 2. Exploración de datos (como cualquier otro df) ####
glimpse(postas_covid)

names(hospitales)
str(hospitales)

tail(comunas)

head(colectivos)

# 3. Joins de datos espaciales (sf::st_join()) ####

?st_crs
st_crs(comunas)
st_crs(hospitales)
df_hosp_com <- st_join(comunas, hospitales)

view(df_hosp_com)

## 3.1. Definir sistema de coordenadas ####

df_postas_com <- st_join(comunas, postas_covid)

st_crs(comunas)
st_crs(postas_covid)
st_crs(postas_covid) <- 4326

df_postas_com <- st_join(comunas, postas_covid)

view(df_postas_com)

# 4.  Visualización de datos vectoriales (ggplot2::geomsf()) ####

### *Sistema de coordenadas geográficas*:  método para describir la posición de una ubicación geográfica en la superficie de 
### la Tierra utilizando mediciones esféricas de latitud y longitud.
### Primero latitud, luego longitud. Ejemplo: Coordenadas de CABA (34° 36′ 30″ S; 58° 22′ 16″ O)

mapa_postas <- ggplot() + geom_sf(data = postas_covid)
mapa_postas


## 4.1. Tipos de representaciones vectoriales ####

### Polígonos ####
mapa_poligonos <- ggplot() + geom_sf(data = comunas)
mapa_poligonos

### Líneas ####
mapa_lineas <- ggplot() + geom_sf(data = colectivos)
mapa_lineas

### Puntos ####
mapa_puntos <- ggplot() + geom_sf(data = hospitales)
mapa_puntos

## 4.2. Visualizar una variable con fill ####

#cantidad de hospitales por comuna: ¿qué pasa con la comuna 1?
df_hosp_com <- df_hosp_com %>% 
  group_by(Comuna) %>% 
  mutate(cantidad = n())

view(df_hosp_com)

#chequear!
df_hosp_com_sin1 <- df_hosp_com %>% 
  filter(!(Comuna == 1)) %>% 
  group_by(Comuna) %>% 
  mutate(cantidad = n())

ggplot() + 
  geom_sf(data = df_hosp_com_sin1, aes(fill = cantidad))

#transformación del dato
df_hosp_com <- df_hosp_com %>% 
  mutate(cantidad = ifelse(Comuna == 1, 0, cantidad))

ggplot() + 
  geom_sf(data = df_hosp_com, aes(fill = cantidad))

## 4.3. Definir o suprimir límites con color ####

ggplot() + 
  geom_sf(data = df_hosp_com, aes(fill = cantidad), color = NA)

ggplot(data = df_hosp_com, aes(fill = cantidad)) + 
  geom_sf(color = "white") 

## 4.4. Definir el ancho de los límites con size y ajustar otros aspectos de la viz ####

ggplot(data = df_hosp_com, aes(fill = cantidad)) + 
  geom_sf(color = "white", size=.5) +
  labs(title = "Cantidad de hospitales, por comuna.",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad de hospitales") +
  scale_fill_viridis_c() +
  theme_void()

## 4.5 Agregar texto al mapa (geom_sf_text() o geom_sf_label()) ####
### Por ejemplo, el nombre de hospitales de aquellas comunas que cuentan con dos hospitales: 

filter(df_hosp_com, cantidad == "2")

ggplot(data = df_hosp_com, aes(fill = cantidad)) + 
  geom_sf(color = "white") +
  geom_sf_text(data = df_hosp_com %>% filter(Comuna %in% c(3,7,10,11,15)),
               aes(label = NOM_MAP, geometry = geometry), size=3, color = "white")+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_c(option = 'C') +
  labs(title = "Cantidad de hospitales, por comuna.",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad de hospitales") +
  theme_void()

# Puede mejorar

#install.packages("ggrepel")
ggplot(data = df_hosp_com, aes(fill = cantidad)) +
  geom_sf() +
  ggrepel::geom_label_repel(data = df_hosp_com %>% filter(Comuna %in% c(3,7,10,11,15)), 
               aes(label = NOM_MAP, geometry = geometry), size=2, stat = "sf_coordinates")+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_c(option = 'C') +
  labs(title = "Cantidad de hospitales, por comuna.",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "Hospitales")+
  theme_void()

## 4.6. Superposición de capas ####

ggplot() +
  #comunas
  geom_sf(data = comunas) + 
  #hospitales
  geom_sf(data = hospitales, color = "blue", size = 2) +
  #postas covid
  geom_sf(data = postas_covid, color = "red", size= 2) +
  labs(title = "Hospitales y Postas COVID de la ciudad",
       subtitle = "Ciudad de Buenos Aires")+
  theme_void()

# Agregamos capa con números de comunas
ggplot() +
  geom_sf(data = comunas) + 
  #nueva capa
  geom_sf_text(data = comunas, aes(label = Comuna), size=3, color = "black") +
  geom_sf(data = hospitales, color = "blue", size = 2) +
  geom_sf(data = postas_covid, color = "red", size= 2) +
  labs(title = "Hospitales y Postas COVID de la ciudad",
       subtitle = "Ciudad de Buenos Aires")+
  theme_void()

## 4.7. Plot ####
### No sólo con ggplot podemos plotear datos gee, también con plot seleccionando variables
### Por ejemplo, el mapa original de cantidad de hospitales por comuna

df_hosp_com %>% 
  #seleccionamos variable a graficar y variable con datos geo
  select(cantidad, geometry) %>% 
  #ancho de los límites
  plot(lwd=.1) 

## 4.8. Otras transformaciones de datos geo ####

### Calcular el area de un polígono (sf::st_area()) ####
#por default es en m2
df_hosp_com$area2 <- st_area(df_hosp_com)
View(df_hosp_com)

### Calcular la longitud de una multilinea (sf::st_length()) ####
colectivos$long_recorridos <- st_length(colectivos)
View(colectivos)

### Otro tipo de join y contabilizar la cantidad de puntos contenidos en un polígono (spatialEco::point.in.poly()) ####

#join
cantidad_hospitales <- point.in.poly(hospitales, comunas)

#conteo
cantidad_hospitales <- table(cantidad_hospitales$Comuna)
cantidad_hospitales <- as.data.frame(cantidad_hospitales) %>% rename(Comuna = Var1, 
                                                                     Cantidad = Freq)

view(cantidad_hospitales)

## 5.  Visualización interactiva de datos vectoriales (Leaflet) ####
#Leaflet es una librería de Javascript con implementación en R 

#Capa 0: (¡Permite el uso de %>%!)
mapa <- leaflet() %>% 
#Capa 1: setear punto donde se va a fijar el zoom
  setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>% 
#Capa 2: mapa base, por defecto estan los de OpenStreetMap
  addTiles()
mapa

### 5.1. Otras capas de tiles (Leaflet::addProviderTiles()) ####

#cartoDB
mapa %>% addProviderTiles(providers$CartoDB.Positron)

#ESRI
mapa %>% addProviderTiles(providers$Esri.WorldStreetMap)

### 5.2. Plotear en leaflet: agrego marcadores en los puntos (addMakers()) ####

#requiere contar con variables que contengan datos de long y lat
hospitales <- hospitales %>%
  mutate(long = unlist(map(hospitales$geometry,1)),
         lat = unlist(map(hospitales$geometry,2)))

head(hospitales)

leaflet(data = hospitales) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~long, ~lat)

### 5.3. Agregar popups y labels en los puntos (addMakers()) ####


head(hospitales,2)

leaflet(data = hospitales) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~long, ~lat, 
             popup = ~as.character(TIPO), 
             label = ~as.character(NOM_MAP)) 


### 5.4. Agregar iconos en los puntos (addMakers()) ####

icono <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/3063/3063176.png",
  #ancho
  iconWidth = 30, 
  #alto
  iconHeight = 55,
  iconAnchorX = 18, iconAnchorY = 84)

leaflet(data = hospitales) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~long, ~lat, 
             icon = icono,
             label = ~as.character(NOM_MAP)) 


### 5.5. Superponer capas: polígonos + puntos (addPolylines()) ####

leaflet(data = hospitales) %>% 
  setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~long, ~lat, 
             label = ~as.character(NOM_MAP))  %>% 
  #nueva capa
  addPolylines(data = comunas, color="#2F4AFF", opacity = 1, weight = 2)


### 5.6. Coropletas en Leaflet (addPolygons()) ####

st_crs(df_hosp_com)

#### Customizar paleta ####
bins <- c(0, 1, 3, 5, 7, 9, 11, 15, Inf)
pal <- colorBin("PuRd", domain = df_hosp_com$cantidad, bins = bins)


mapa <- leaflet(data = df_hosp_com) %>% 
  setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
      fillColor = ~pal(cantidad),
    #tamaño de limites
    weight = 2,
    opacity = 1,
    color = "white",
      #tipo de linea
      dashArray = "5",
      fillOpacity = 0.7
    )

mapa

#### Labels ####
labels <- sprintf(
  "<strong>%s</strong><br/> Cantidad de hospitales: %g <sup></sup>",
  df_hosp_com$Comuna, df_hosp_com$cantidad) %>% lapply(htmltools::HTML)

#### Destacar polígonos (highlight = highlightOptions()) ####
mapa <- mapa %>% 
  addPolygons(
  fillColor = ~pal(cantidad),
  weight = 2, 
  opacity = 1, 
  color = "white", 
  dashArray = "5", 
  fillOpacity = 0.7,
  #destaco! 
  highlight = highlightOptions(
    weight = 5, 
    color = "#666", 
    dashArray = "", 
    fillOpacity = 0.7, 
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"))


#### Agregar leyenda (addLegend()) ####
mapa <- mapa %>% addPolygons(
  fillColor = ~pal(cantidad),
  weight = 2, opacity = 1, color = "white", dashArray = "5", fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
  addLegend("bottomright", pal = pal, values = ~cantidad, title = "Cantidad de hospitales", opacity = 1)

### 5.7. Exportar mapa ####

#install.packages("htmlwidgets")
htmlwidgets::saveWidget(mapa,
                        "/Users/Melina/OneDrive/Documentos/Proyectos R/Curso-R-m2/Mapa_hospitales.html",
                        selfcontained = T)

### 6.YAPA. Customizar los iconos según categorías de alguna variable (addAwesomeMarkers()) ####

#recategorizo variable que definirá color del ícono
hospitales <- hospitales %>%
  mutate(TINUM = case_when(
    TIPO == "Hospital de niños"~ "1",
    TIPO == "Hospital especializado"~ "2",
    TIPO == "Hospital de agudos"~ "3"))
hospitales$TINUM <- as.numeric(hospitales$TINUM)

# creo una función que define el tipo de color del icono

getColor <- function(hospitales) {
  sapply(hospitales$TINUM, function(TINUM) {
    if(TINUM == 1) {
      "green"
    } else if(TINUM == 2) {
      "red"
    } else {
      "orange"
    } })
}

#defino el ícono
icons <- awesomeIcons(
  icon = 'ios-close',
  library = 'ion',
  markerColor = getColor(hospitales))

#ploteo! 
leaflet(data = hospitales) %>% 
  setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(~long, ~lat, icon = icons, label = ~as.character(TIPO)) %>%
  addPolylines(data = comunas, color="#2F4AFF", opacity = 1, weight = 2)


# Práctica ####

# 1. Importar el geodf de establecimientos educativos de cABA: https://data.buenosaires.gob.ar/dataset/establecimientos-educativos

# 2. Plotear la distribución de las escuelas en la ciudad

# 3. Customizar al menos 3 aspectos del mapa 

# 4. Joinear el df de establecimientos educativos con el de barrios de la ciudad: https://data.buenosaires.gob.ar/dataset/barrios

# 5. Calcular la cantidad de establecimientos que hay por barrio 

# 6. En un gráfico interactivo: 

# 6.A. Visualizar la ubicación de los establecimientos en la ciudad y brindar información en el mapa del nivel que ofertan

# 6.B. Crear una coropleta con la cantidad de establecimientos por barrio
