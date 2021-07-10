#################################
# PROCESAMIENTO DE DATOS GRILLADOS DE PRECIPITACION PISCO v2.1 - SENAMHI
# CURSO: PROGRAMACION CON R
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
#################################

#---------------------------------------------------------
# Para limpiar la consola:
# TeclaS :  Ctrl + L

# Para limpiar el workspace:
rm(list = ls())

###############
#  Paquetes   #
###############

library(raster)
library(sf)
library(ncdf4)
library(ggplot2)
library(ggspatial)
library(devtools)
library(basemapR)
library(cowplot)
library(rgdal)
library(DataExplorer)

# FIJAR EL DIRECTORIO DE TRABAJO

setwd("C:/Users/Asus/Desktop/R/AVANZADO/CLASE2") 

#######################
#  DESCARGA DE DATOS  #
#######################

# El producto grillado de precipitacion, temperatura y caudales PISCO 
# fue construido por los investigadores del SENAMHI, y tiene datos para todo el Perú.

# Artículos relacionados a PISCO: 
# https://www.senamhi.gob.pe/load/file/01402SENA-8.pdf
# https://doi.org/10.1080/02626667.2019.1649411

# Los datos se encuentran en la biblioteca de datos climaticos IRI/LDEO
# https://iridl.ldeo.columbia.edu/

download.file("https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Prec/.v2p1/.stable/.daily/.Prec/data.nc",
              "PISCOpp_V2.1b.nc")


###################
#  IMPORTAR DATOS #
###################


# IMPORTAR EL ARCHIVO NETCDF (.nc) DE PISCO

PISCOpp_d <- raster::brick("PISCOpp_V2.1.nc")
PISCOpp_d

# CONOCER EL SISTEMA DE REFERENCIA DE COORDENADAS (CRS)

projection(PISCOpp_d)
crs(PISCOpp_d) <- CRS('+init=EPSG:4326')
spplot(PISCOpp_d[[1:10]])

# IMPORTAR LOS SHP DE CUENCAS HIDROGRAFICAS Y ESTACIONES METEOROLOGICAS DEL PERU

cuencas_shp <- read_sf('CUENCAS-PERU.shp')
estaciones_shp <- st_read('ESTACIONES-HIDROMETEO-PERU.shp')

###########################################
cuencas_shp2 <- readOGR('CUENCAS-PERU.shp')

# IMPORTAR LOS SHP DE LIMITES GEOGRAFICOS

peru_lim_shp <-getData('GADM', country='PER', level=1)
plot(peru_lim_shp)

peru_lim_shp <- st_as_sf(peru_lim_shp)

# VISUALIZAR EL SHP DE LAS ESTACIONES

map_estaciones <- ggplot() + 
  geom_sf(data = peru_lim_shp) +
  theme_bw() + xlim(-85,-65) + ylim(-20,2) + 
  xlab("Longitud") + ylab("Latitud") +
  geom_sf(data = estaciones_shp, aes(color = ico), size = 0.5) +
  scale_color_manual(values=c("blue", "red"))

map_estaciones

map_estaciones <- ggplot() +
  borders("world", colour="black", fill= "Beige") +
  theme_bw() + xlim(-85,-65) + ylim(-20,2) + 
  xlab("Longitud") + ylab("Latitud") +
  geom_sf(data = estaciones_shp, aes(color = ico)) +
  scale_color_manual(values=c("blue", "skyblue"))

map_estaciones

#####################
#  GEOPROCESAMIENTO
#####################

# OBTENCION DE UN OBJETO SOLO CON LAS EST METEOROLOGICAS

est_met <- estaciones_shp[estaciones_shp$ico == "M",]

# OBTENCION DE UN OBJETO SOLO CON LA CUENCA RIMAC

cuenca_rimac <- cuencas_shp[!is.na(cuencas_shp$NOMB_UH_N6) & cuencas_shp$NOMB_UH_N6 == "Rimac",]

# APLICAR LA HERRAMIENTA DE RECORTE O "CLIP"
#output = st_intersection(input, clip)

est_met_rimac <- st_intersection(est_met, cuenca_rimac)

A <- ggplot() + 
  base_map(st_bbox(cuenca_rimac), basemap = 'mapnik',increase_zoom = 3) +
  geom_sf(data = cuenca_rimac, color = "black", fill = NA) +
  geom_sf(data = est_met_rimac, aes(color = nom), shape = 10, size = 5) +
  theme_bw() +
  xlab("Longitud") + ylab("Latitud") +
  labs(color = "Estaciones meteorológicas") +
  annotation_north_arrow(location="tr", which_north="true", 
                         style= north_arrow_nautical())+
  annotation_scale(location = "bl",bar_cols = c("darkgray", "white"),
                   style = "ticks")


leaflet( ) %>%
  addTiles() %>%
  addRasterImage(raster_rimac1, opacity = 0.6) %>%
  addPolygons(data = cuenca_rimac,
              fillColor = terrain.colors(10, alpha = NULL),
              popup = ~as.character(Nombre_UH)) 



B <- ggplot() + 
    geom_sf(data = peru_lim_shp, color = "black", fill = "beige") +
    scale_x_continuous(breaks=seq(-85,-65,5)) +
    ggtitle("MAPA DE UBICACIÓN") +
    theme_bw() +
    theme(plot.title = element_text(size = 6, hjust = 0.5),
          axis.text = element_text(size = 6))


mapa_2 <- ggdraw() + 
  draw_plot(A) + 
  draw_plot(B, x = -0.35, y = 0.3, scale = 0.3)
 
mapa_2

         
########################
#  EXTRACCION DE DATOS
########################

# 1. EXTRAER LOS DATOS DE PISCO SENAMHI PARA LAS ESTACIONES DE LA CUENCA


# CADA ESTACION ESTA UBICADA EN UN PIXEL ESPECIFICO, EL CUAL TIENE UN ID

cell_id_est <- raster::extract(PISCOpp_d[[5]], 
                                   est_met_rimac, 
                                   cellnumbers = T)[,1]

# OBTENEMOS SOLO LOS DATOS DE LOS PIXELES DE CADA ESTACION 

data_pisco_rimac <- t(PISCOpp_d[cell_id_est])

# CONVERTIMOS LA MATRIZ A DATAFRAME Y ASIGNAMOS LOS NOMBRES DE LAS ESTACIONES

df_pisco_rimac <- as.data.frame(data_pisco_rimac)
colnames(df_pisco_rimac) <- as.character(est_met_rimac$nom)

# AGREGAMOS UNA COLUMNA CON LAS FECHAS

df_pisco_rimac$date <- seq(as.Date("01-01-1981", format = "%d-%m-%Y"), 
                           length.out = nrow(df_pisco_rimac), 
                           by = "day")

df_pisco_rimac <- df_pisco_rimac[,c(25,1:24)]
row.names(df_pisco_rimac) <- NULL


# 2. EXTRAER LOS DATOS DE PISCO SENAMHI PARA TODA LA EXTENSION DE LA CUENCA

# IMPORTAR EL ARCHIVO NETCDF (.nc) DE PISCO prec mensual

PISCOpp_m <- raster::brick("PISCOppm_V2.1.nc")
PISCOpp_m

crs(PISCOpp_m) <- CRS('+init=EPSG:4326')
projection(PISCOpp_d)

# CORTAR EL RASTER USANDO UN POLIGONO (EXTRACT BY MASK)
# DE SER NECESARIO GENERAMOS UN BUFFER PREVIO

buff_5km_rimac <- st_buffer(cuenca_rimac, 0.15)

raster_rimac1 <- raster::mask(PISCOpp_d[[1]],
                              buff_5km_rimac)

spplot(raster_rimac1)

# CONVERTIMOS A DATAFRAME EL RASTER PARA SU PLOTEO

df_raster_rimac1 <- raster::as.data.frame(raster_rimac1, xy = TRUE)
df_raster_rimac1 <- na.omit(df_raster_rimac1)
colnames(df_raster_rimac1) <- c("x", "y", "PP_ENERO")

ggplot() +
  geom_raster(data = df_raster_rimac1 , aes(x = x, y = y, fill = PP_ENERO)) +
  geom_sf(data = cuenca_rimac, fill = NA, color = "black") +
  geom_sf(data = est_met_rimac , shape = 10, size = 5) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("PRECIPITACIÓN ACUMULADA MENSUAL DE LA CUENCA RIMAC - ENERO 1981")
  


#####################################################


raster_rimac1 <- raster::crop(PISCOpp_d[[1]],
                              cuenca_rimac)

spplot(raster_rimac1)

# CONVERTIMOS A DATAFRAME EL RASTER PARA SU PLOTEO

df_raster_rimac1 <- raster::as.data.frame(raster_rimac1, xy = TRUE)
df_raster_rimac1 <- na.omit(df_raster_rimac1)
colnames(df_raster_rimac1) <- c("x", "y", "PP_ENERO")

ggplot() +
  base_map(st_bbox(cuenca_rimac), basemap = 'mapnik',increase_zoom = 3) +
  geom_raster(data = df_raster_rimac1 , aes(x = x, y = y, fill = PP_ENERO)) +
  geom_sf(data = cuenca_rimac, fill = NA, color = "red") +
  geom_sf(data = est_met_rimac , shape = 10, size = 5) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("PRECIPITACIÓN ACUMULADA MENSUAL DE LA CUENCA RIMAC - ENERO 1981")








