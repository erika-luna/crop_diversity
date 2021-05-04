# Load packages
library(geojsonio)
library(mapproj)

spdf <- geojson_read("/Users/erikaluna/R\ Studio/crop_diversity/spatial_data/mexico_states/json/dest_2015gw.json",  what = "sp")

plot(spdf)

# I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
library(broom)
spdf_fortified <- tidy(spdf, region = "code")

# Now I can plot this shape easily as described before:
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf, aes( x = long, y = lat), fill="white", color="grey") +
  theme_void() +
  coord_map()


# Geospatial data available at the geojson format
library(geojsonio)
spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")

# Since it is a bit too much data, I select only a subset of it:
spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("06", "83", "13", "30", "34", "11", "66") , ]
# Geospatial data available at the geojson format
library(geojsonio)
spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")

# Since it is a bit too much data, I select only a subset of it:
spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("06", "83", "13", "30", "34", "11", "66") , ]

# I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
library(broom)
spdf_fortified <- tidy(spdf, region = "code")

# Now I can plot this shape easily as described before:
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()


#### M??xico #####
#Algoridmo de descarga de los datos.
url<-"http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463142683_s.zip"
if(!file.exists("shpmexico.zip")){
  download.file(url,"shpmexico.zip")
}
if(!file.exists("conjunto_de_datos/areas_geoestadisticas_basicas_rurales.dbf")){
  unzip("shpmexico.zip")
}

library(rgdal)
library(geojsonio)
library(spdplyr)
library(rmapshaper)
library(jsonlite)
library(highcharter)

#Leer los mapas
area<-readOGR("conjunto_de_datos/areas_geoestadisticas_municipales.shp",
              layer="areas_geoestadisticas_municipales",
              verbose = FALSE)
#Mostrar los datos del mapa
head(area@data)

#Cambiar nombre de los datos
area<-area%>%rename(state_code=CVE_ENT,mun_code=CVE_MUN,
                    mun_name=NOM_MUN)
#Mostrar los datos del mapa
head(area@data)

area<-area%>%mutate(state_code=as.factor(as.numeric(as.character(state_code))),
                    mun_code=as.factor(as.numeric(as.character(mun_code))))
#Mostrar los datos del mapa
head(area@data)

#plot(area)

#Convertir a JSON
area_json<-geojson_json(area)
#guardar
geojson_write(area_json,file = "todosmun.json")


######
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")
                 

