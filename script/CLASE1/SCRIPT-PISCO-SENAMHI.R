#################################
# EXTRACCION DE DATOS DE PISCO SENAMHI
# CURSO: PROGRAMACION CON R
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
#################################

#---------------------------------------------------------
# Para limpiar la consola:
# TeclaS :  Ctrl + L

# Para limpiar el workspace:
rm(list = ls())


# 1. RESETEO DE VARIABLES

rm(list = ls())

# 2. SELECCIONAR NUESTRA CARPETA DE TRABAJO

setwd("C:/Users/")  

# 3. INSTALACION DE PAQUETES O LIBRERIAS

install.packages("raster")
install.packages("ncdf4")

# 4. CARGAR O IMPORTAR LOS PAQUETES

library(raster)
library(ncdf4)

# 5. IMPORTAR LAS COORDENADAS 

long_lat <- read.csv("COORDENADAS.csv", header = T, 
                     sep = ";")

# 6. DESCARGA DE DATOS PISCO

raster_pp <- raster::brick("PISCOpd.nc")
sp::coordinates(long_lat) <- ~XX+YY

raster::projection(long_lat) <- raster::projection(raster_pp)
points_long_lat <- raster::extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat <- t(raster_pp[points_long_lat])
colnames(data_long_lat) <- as.character(long_lat$NN)

# 7. GUARDAR EL ARCHIVO

save.image("C:/Users/data_long_lat.csv")
write.csv(data_long_lat, "data_long_lat.csv", quote = F)

