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

area<- ms_simplify(area)
plot(area)

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
                 
##### nepal #### This works
library(sf)
library(tidyverse)

nepal_shp <- read_sf('/Users/erikaluna/R\ Studio/crop_diversity/spatial_data/mexico_states/json/dest_2015gw.json')
nepal_data <- read_csv('/Users/erikaluna/R\ Studio/crop_diversity/coefs_state.csv')

# calculate points at which to plot labels
centroids <- nepal_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

h <- nepal_data %>% 
  #filter(`Sub Group` == "HPI") %>% 
  #mutate(District = toupper(District)) %>% 
  left_join(nepal_shp, ., by = c('COV_ID' = 'COV_ID')) %>% 
  ggplot() + 
  geom_sf(aes(fill = year)) #+ 
  #geom_text(aes(X, Y, label = year), data = centroids, size = 1, color = 'white')

#### 
# Read this shape file with the rgdal library. 
library(rgdal)
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/spatial_data/mexico_mun") , 
  layer="muni_2018cw",
  verbose=FALSE
)

# Clean the data object
library(dplyr)
head(world_spdf@data)
world_spdf<-world_spdf%>%rename(state_code=CVE_ENT,mun_code=CVE_MUN,
                    mun_name=NOM_MUN)
#Mostrar los datos del mapa
head(world_spdf@data)


world_spdf<-world_spdf%>%mutate(state_code=as.factor(as.numeric(as.character(state_code))),
                    mun_code=as.factor(as.numeric(as.character(mun_code))))

# -- > Now you have a Spdf object (spatial polygon data frame). You can start doing maps!

# Library
library(leaflet)

# Create a color palette for the map:
mypalette <- colorFactor( palette="viridis", domain=world_spdf@data$mun_code, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(mun_code), stroke=FALSE )

m


# load ggplot2
library(ggplot2)

# Distribution of the population per country?
world_spdf@data %>% 
  ggplot( aes(x=as.numeric(POP2005))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  xlab("Population (M)") + 
  theme_bw()

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet1.html"))

###### UTF8 
mun_name <-as.data.frame(world_spdf@data@mun_name)
iconv(world_spdf@data@mun_name, from = "spanish", to = "utf8", sub = NA, mark = TRUE, toRaw = FALSE)

##### convert mun code
cov_id <- 1:2463

tmp <- SIAP_mun %>% 
 # group_indices(state_code, mun_code)
  
  group_by(state_code, mun_code) %>% 
  #group_by(state_code, mun_code) %>% 
  mutate(cov_id = group_indices(mun_code))

count = row_number(IDFAM)

df %>% group_by(IDFAM) %>% mutate(count = sequence(n()))


coefs_mun <- read.csv("coefs_mun.csv")
colnames(coefs_mun) <- c("mun", "intercept", "slope")
SIAP_coef <- left_join(SIAP_mun, coefs, by = "mun")

#### second try i think this one works
SIAP_mun <- SIAP_mun %>% 
  mutate(ID = group_indices(., state_code, mun_code))



