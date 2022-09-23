
library(tidyverse)
library(here) #refiere la ruta a la carpeta del proyecto
library(tidylog) #informa sobre operaciones dplyr y tidyr
#install.packages("summarytools")
library(summarytools) #resume de forma clara y rápida datos numéricos y categóricos
library(knitr) 
library(MASS)
library(effects)

##########################
### GAM  ###

# en este caso en lugar de una unica pendiente tenemos una funcion suavizada
# son modelos muy flexibles 

library(mgcv)

# el modelo se escribe:
#m <- gam(d15N ~ s(Year, k = 15), data = isotopes, method = ”REML”)

# funcion gam.check: nos da informacion sobre el ajuste de la k

# podemos introducir factores de autocorrelacion temporal o espacial
# mod <- gamm(d15N ~ s(Year, k = 15), data = isotopes,
#            correlation = corCAR1(form = ~ Year), method = ”REML”)


# tambien se pueden introducir interacciones para ello en lugar de S se pone t
# b <- gamm(y~te(x0,x1),data=dat)


# ejercicio
mort<-read.csv(here("Data/UN_GDP_infantmortality.csv"))
head(mort)
levels(factor(mort$country))

plot(mort$gdp, mort$infant.mortality)


m_mort_bn<-glm.nb(infant.mortality ~ gdp, data = mort)
summary(m_mort_bn)
performance::check_model(m_mort_bn)
plot(allEffects(m_mort_bn))

mort$log.gdp <- log(mort$gdp)
mort.gam <- gam(infant.mortality ~ s(log.gdp), family = nb, data = mort)
summary(mort.gam)


## GAMMM ##
library(lme4)

# sgamm <- gam(Reaction ~ s(Days, Subject, k = 3, bs = ”fs”),
# data = sleepstudy, method = ”REML”)



#################################################
### Bayesian modelling  ### ----

library(brms) # usa formula para hacer analisis bayesiano

trees <- read.csv(here("Data/trees.csv"))

#centra variable en torno a 25
trees$dbh.c <- trees$dbh - 25

# definir la formula
height.formu <- brmsformula(height ~ dbh.c)

# We must define prior distributions for every parameter
# el paquete te marca unas por defecto
get_prior(height.formu, data = trees) 

# yo defino las prior
priors <- c(
  set_prior("normal(30, 10)", class = "Intercept"),
  set_prior("normal(0.5, 0.4)", class = "b"),
  set_prior("normal(0, 5)", class = "sigma")
)

# correr el modelo sin datos solo con las prior para ver si tiene sentido
height.mod <- brm(height.formu,
                  data = trees,
                  prior = priors,
                  sample_prior = "only")

#correr el modelo con los datos
height.mod <- brm(height.formu,
                  data = trees,
                  prior = priors)


# el factor Rhat es muy importante y debe estar alrededor a 1

#es importante hace el plot del modelo para ver las cadenas 


##################################################3
####  GIS con R ### ----

library(knitr)


library(dplyr)
covid <- readr::read_csv("Data/covid.csv")
muni.coords <- readr::read_csv("Data/coords_towns.csv") # datos de coordenadas
covid <- left_join(covid, muni.coords, by = "Municipio")
covid


# quiero hacer un objeto espacial
library(sf)

covid.sf <- st_as_sf(covid) # error no encuentra las coordenadas, hay que indicarselo
covid.sf <- st_as_sf(covid, coords = c("x", "y"))
covid.sf   # ahora ya tenemos un objeto espacial 
# las coordenadas estan en UTM
# CRS = NA
# hay que especificar que sistema de referencia es

# Setting the Coordinate Reference System (CRS)
# Search EPSG, e.g. at https://spatialreference.org/
# For UTM 30N datum ETR89, EPSG = 25830

covid.sf <- st_set_crs(covid.sf, value = 25830)
covid.sf

# Change projection
# From UTM 30N (EPSG = 25830) to lonlat (EPSG = 4326)

covid.geo <- st_transform(covid.sf, crs = 4326)
covid.geo

# para obtener las coordenadas
st_coordinates(covid.geo)


covid.sf

# Manipulating sf objects with dplyr
covid.sf %>%
  filter(Provincia== "Huelva")

covid.sf %>%
  filter(Provincia== "Granada", Casos > 100)

covid.sf %>%
  group_by(Provincia)  %>%
  summarise(sum(Casos))


# hacer mapa
plot(covid.sf) # basico con informacion basica


# mapa interactivo
library(mapview)
mapview(covid.sf)

mapview(covid.sf, zcol = "Casos")  # zcol es la variable que queremos representar

mapview(covid.sf, zcol = "Fallecidos", cex = "Fallecidos") # cex= tamaño del punto

# mapas con ggplot (puedes modificar todo igual que en un grafico ggplot)
library(ggplot2)

ggplot(covid.sf) + geom_sf()
ggplot(covid.sf) + geom_sf(aes(colour = Casos)) +
  theme_minimal()

# tmap package
library(tmap)

tm_shape(covid.sf) +    # specify spatial object
  tm_symbols()          # map points

tm_shape(covid.sf) +
  tm_symbols(size = "Casos")

tm_shape(covid.sf) +
  tm_symbols(size = "Casos" ,
             col = "orange", border.col = "orange")

tm_shape(covid.sf) +
  tm_symbols(size = "Casos",
             col = "orange", border.col = "orange",
             title.size = "COVID cases per town") +
  tm_layout(frame = FALSE, legend.title.size = 0.9) +
  tm_compass(type = "8star", position = c(0.04, 0.2), size = 2) +    #añade rosa del viento
  tm_scale_bar(width = 0.15, position = c(0, 0.03))                  # añade la escala


#Adding base map
library(maptiles)
library(terra)

bmap <- get_tiles(covid.sf, zoom = 7, crop = TRUE)
plot(bmap)

# provider = poner el tipo de mapa que queremos bajar 
bmap <- get_tiles(covid.sf, provider = "CartoDB.Positron" , zoom = 7, crop = TRUE)
plot(bmap)

# añadiendo informacion
tm_shape(bmap) +
  tm_rgb(alpha = 0.3) +
  tm_shape(covid.sf) +
  tm_symbols(size = "Casos", scale = 1.5, title.size = "Casos por municipio") +
  tm_layout(legend.position = c("right", "bottom"),
            inner.margins = c(0.03, 0.01, 0.02, 0.01)) +
  tm_credits(get_credit("Stamen.Watercolor"), size = 0.4, position = c(0.02, 0))

# se pueden combinar distintos mapas con funcion tmap_arrange(map.1, map.2)

# Saving tmap objects
tmap_save(mymap,"mymap.png") # raster image
tmap_save(mymap,"mymap.svg") # vector
tmap_save(mymap,"mymap.html") # html (interactive, animated...)


# sf can read multiple spatial data types
#puede leer datos espaciales 
munis <- st_read("Data/municipios_lite.gpkg")

# ahora tenemos poligonos
tmap_mode("plot")
tm_shape(munis) + tm_polygons()

# To join a spatial (sf)object with plain dataframe,use normal join
# creo un objeto donde cada poligono tiene el numero de casos de los datos 
covid.plain <- covid.sf %>%
  st_drop_geometry() %>%
  rename(MUNICIPIO = Municipio)

covid.town <- st_join(munis, covid.sf)


# plot
tmap_mode("plot")
tm_shape(munis) +
  tm_polygons(col = "white") +
  tm_shape(covid.sf) +
  tm_dots()

tm_shape(covid.town) +
  tm_polygons(col = "Casos", border.alpha = 0.05, legend.is.portrait = FALSE)

# Number of cases per province
cases.province <- covid.town %>%
  group_by(PROVINCIA) %>%
  summarise(total.cases = sum(Casos, na.rm = TRUE))
cases.province

tm_shape(cases.province) +
  tm_fill(col = "total.cases")


# Get borders of Andalucia
# st_union combines geometries (dissolve polygons)
andal <- st_union(munis)
andal

#usando dplyr
grazalema <- munis %>%
  filter(MUNICIPIO == "Grazalema")
grazalema

sevilla <- cases.province %>%
  filter(PROVINCIA == "Sevilla")
sevilla

# ¿grazalema esta dentro de Sevilla?
st_within(grazalema, sevilla, sparse = FALSE)
# Does Grazalema share border with Sevilla province?
st_touches(grazalema, sevilla, sparse = FALSE)
#Is Grazalema withi n 50 km of Sevilla province?
st_is_within_distance(grazalema, sevilla, dist = 50000, sparse = FALSE)

# genera buffer
sev.muni <- munis %>%
  filter(MUNICIPIO == "Sevilla (capital)")
sev.metro <- st_buffer(sev.muni, dist = 20000)  # buffer de 20 km (unidades en funcion de la capa)
sev.metro

tmap_mode("view")
tm_shape(sev.metro) +
  tm_fill() +
  tm_shape(sev.muni) +
  tm_fill(col = "black")
tmap_mode("plot")

#Find which towns intersect metropolitan area

sev.munis <- munis %>%
  filter(PROVINCIA == "Sevilla")
tm_shape(sev.munis) +
  tm_polygons()

intersect.metro <- st_join(sev.munis, sev.metro, left = FALSE)
intersect.metro

tm_shape(sev.munis) +
  tm_polygons(col = "white") +
  tm_shape(intersect.metro) +
  tm_polygons(col = "yellow") +
  tm_shape(sev.metro) +
  tm_fill(alpha = 0.7)


###### Raster data ######

# dos paquete principales para trabajar con raster
# package terra y package stars

library(terra)
tmin.sp <- rast("Data/tminSp.tif")
tmin.sp

# visualizacion rapida
plot(tmin.sp)

#Tmin January
plot(tmin.sp, y = 1)

# cambiar colores
library(RColorBrewer)
plot(tmin.sp, y = 1,
     col = brewer.pal(9, "Blues"))


#Adding vector shapes
andal.sf <- sf::st_read("data/Andalucia_contorno.gpkg" , quiet = TRUE)
plot(tmin.sp, y = 1, axes = FALSE,col = RColorBrewer::brewer.pal(9, "Blues"))
plot(vect(andal.sf), add = TRUE)


#Plotting rasters with tmap
library(tmap)
tm_shape(tmin.sp) +
  tm_raster()

# otro paquete para raster 
library(rasterVis)
levelplot(tmin.jan)

# este paquete te permite generar un objeto ggplot
library(ggplot2)
gplot(tmin.jan) +
  geom_tile(aes(fill = value)) +
  cowplot::theme_map()


# hacer calculos
library(geodata)
tmax.sp <- worldclim_country(country = "Spain", var = "tmax", path = "data/")
tmax.sp

tmin.jan <- subset(tmin.sp, 1)
tmax.jan <- subset(tmax.sp, 1)
tmean.jan <- mean(tmin.jan, tmax.jan)

# cortar con crop
tmin.jan.crop <- crop(tmin.jan, ext(-8, -1, 36, 39))
plot(tmin.jan.crop)

# cortar con paquete sf
library(sf)
and.lim <- vect(andal.sf)
tmin.jan.crop <- crop(tmin.jan, and.lim)
plot(tmin.jan.crop)

# te elimina el resto de provicias 
tmin.jan.crop <- mask(tmin.jan.crop, and.lim)
plot(tmin.jan.crop)

#Extract raster values per polygon
tmin.prov <- extract(tmin.andal, vect(andal.provs.geo))
head(tmin.prov)


# ejemplo
aemet <- st_read("Data/aemet/Estaciones_Automaticas.shp" , quiet = TRUE)
head

tm_shape(aemet) + tm_dots()

tm_shape(aemet) + tm_dots()

# solo andalucia
aemet.andal <- st_filter(aemet, andal.sf)
tm_shape(aemet.andal) +
  tm_dots()

# Extract minimum Januaryt emperature for AEMET stations
aemet.tmin <- extract(tmin.jan, vect(aemet.andal))
aemet.tmin <- bind_cols(aemet.andal, aemet.tmin)
head(aemet.tmin)


# se pueden hacer modelos (en este caso un gam)
library(mgcv)
model <- gam(January ~ s(elevSp), data = aemet.tmin.elev)
model

# mapa de elevacion
elev.andal <- crop(elev, and.lim)
elev.andal <- mask(elev.andal, and.lim)
levelplot(elev.andal, margin = FALSE)
