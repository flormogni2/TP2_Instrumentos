#TP 2. Instrumentos de analísis territorial

#Levantamos las librerías necesarias para trabajar en el TP2

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(geoAr)
library(geofacet)
library(stringr)


##En este trabajo práctico se analizará la cantidad de comisarias existentes en la Ciudad de Buenos Aires y su distribución en los barrios. 

#Para ello, levantamos y utilizamos la base de comisarias provistas por el portal de datos abiertos de la Ciudad de Buenos Aires BA DATA. 

base_comisarias <- vroom::vroom("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv")

#visualizamos los datos
summary(base_comisarias)

#Limpiamos la base y eliminamos las columnas que no son de interés
base_comisarias <- base_comisarias %>%
  select(-observaciones, -observaciones_2, -calle2, -telefonos)

#Observamos las columnas que quedaron en el dataset
head(base_comisarias)

#La base de datos posee 49 observaciones, que corresponden a las comisarías y a sus edificios anexos. También se observan 11 variables que muestran las características de esas comisarias, tales como su barrio y comuna, entre otros.  

class(base_comisarias$barrio)

# Convertimos la columna de barrio a factor para poder realizar el análisis estadístico posterior. 
base_comisarias$barrio <- as.factor(base_comisarias$barrio)

# Verificamos el tipo de datos después de la conversión
class(base_comisarias$barrio) 

#Levanto los datos de los barrios
zonas <- sf::st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/comisarias-policia-ciudad/comisarias-policia-de-la-ciudad.geojson")

#Limpiamos la base de zonas, antes de unirla con la base de comisarias. Para ello, hacemos coincidir los valores dentro de la columna de barrio para que queden todos en mayúscula. 
zonas$barrio <- toupper(zonas$barrio)

#Le quito las columnas que no interesan al análisis
zonas <- zonas %>%
  select(-observacio, -calle2, -nom_3, -nom_2, -telefonos)

#Uno las dos bases, para poder tener los datos geográficos.  

comisarias_geo <- left_join(base_comisarias, zonas, by = "id")

#Elimino los datos vacios
comisarias_geo <- na.omit(comisarias_geo)

#Observamos que, finalmente, tenemos 36 estaciones de policia. 

class(comisarias_geo$barrio.x) 

#Levanto la base de los barrios del portal abierto del Gobierno de la Ciudad de Buenos Aires para poder vincularlos geoespacialmente. 

Barrios <- st_read("barrios_data/barrios_wgs84.shp", 
                   stringsAsFactors = TRUE,
                   options = "ENCODING=UTF8")

#El dataset de barrios posee 48 registros y 6 columnas que indican el nombre del barrio, la comuna a la que pertenece, la superficie y la geometría en polígonos. 
#Lo graficamos

ggplot()+
  geom_sf(data=Barrios)+
  theme_minimal()

#Limpiamos los dataset para que las columnas que poseen en común los datasets se llamen de la misma manera para poder unirlos. 

comisarias_geo <- comisarias_geo %>% 
  rename(BARRIO = barrio.x)

#Uno la base de comisarias con la base de los barrios
comisarias_caba <- left_join(comisarias_geo, Barrios, by = "BARRIO")

#Convierto el dataframe a una base de datos geoespacial. 
comisarias_caba <- st_as_sf(comisarias_caba)

class(comisarias_caba)

#Grafico

ggplot()+
  geom_sf(data=Barrios, color="blue")+
  geom_point(data=comisarias_caba, aes(x=long, y=lat), size=0.5, alpha=0.4)

####
comisarias_barrio <- comisarias_caba %>% 
  group_by(BARRIO) %>% 
  summarise(cantidad=n())

#Realizo un gráfico de barras con los principales barrios con mayor cantidad de comisarías. 

ggplot()+
  geom_bar(data=comisarias_barrio %>%
             top_n(5, cantidad), aes(x=reorder(BARRIO, -cantidad), weight=cantidad), fill="#52796f")+
  labs(title="Cantidad de comisarias por barrio - Top 5",
       x="Barrio",
       y="Cantidad de comisarias")+
  theme_minimal()

#Armo un dataset nuevo agrupado por barrios para poder graficarlo y le quito la georeferenciación para poder unirla después. 

comisarias_barrio <- comisarias_barrio %>% select(-geometry.x) %>%
  st_drop_geometry()

#Uno la nueva base con los datos georreferenciales. 

comisarias_barrio_geo <- left_join(comisarias_barrio, Barrios, by="BARRIO") 

comisarias_barrio_geo <- st_as_sf(comisarias_barrio_geo)

# Lo visualizamos a partir de las comisarías en los barrios. 

ggplot() +
  geom_sf(data = comisarias_barrio_geo, aes(fill = cantidad), color = "white") +
  labs(title = "Comisarías por barrio",
       subtitle = "Barrios de CABA",
       fill = "Cantidad",
       caption = "Fuente: BA DATA") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  facet_wrap(~BARRIO, ncol = 7) +
  theme(axis.title.x = element_blank(),  
        axis.text.x = element_blank(),   
        axis.title.y = element_blank(),  
        axis.text.y = element_blank())

#Lo visualizamos en un mapa coroplético para ver cómo se distribuyen en la ciudad.  
ggplot()+
  geom_sf(data=comisarias_barrio_geo, aes(fill=cantidad), color="white")+
  labs(title = "Cantidad de comisarias",
       subtitle = "Barrios de CABA",
       fill = "Cantidad",
       caption= "Fuente: BA DATA") +
  scale_fill_distiller(palette = "Teal", direction = 1) +
  theme_minimal()

#Para realizar un análisis más en profundidad de la implicancia de esta distribución en la Ciudad Autónoma de Buenos Aires, se debería cruzar este análisis con los delitos que se reportan o con la cantidad de habitantes por barrio de CABA. 
