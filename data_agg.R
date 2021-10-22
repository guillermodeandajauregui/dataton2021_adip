################################################################################
#integrar todos los datos procesados 
################################################################################
library(tidyverse)
library(archive)
library(vroom)
library(sf)
library(tidymodels)

#shape file ----
ageb_shp   <- sf::st_read(dsn = un_path) %>% filter(CVE_ENT=="09") %>% st_transform(crs = 4269) 


#marginacion ----
imu <-  "IMU_2020.xls" #un_path # path al archivo descargado de "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/IMU_2020.zip"
shp_path <- "XXXX" # path local al shp file de AGEBs 

imu_data <- readxl::read_excel(path = imu, sheet = 2) %>% filter(ENT=="09")

#procesados por el grupo ----

#poblaciones ---- 
pop_ageb     <- vroom("intermediates/pop_ageb_urbana.txt")
pop_ageb.fem <- vroom("intermediates/pop_ageb_urbana_fem.txt")

#incidencia delitos -----
#delitos_map   <- vroom("intermediates/delitos_map_ageb.tsv")
delitos_100k  <- vroom("intermediates/delitos_100k_ageb.tsv")

#distancias ----

dist_apoyos    <- vroom("intermediates/distancias_apoyoMujeres.txt")

################################################################################
#unir todo en un solo objeto ----

ageb_shp <- left_join(ageb_shp, imu_data, by=c("CVEGEO"="CVE_AGEB"))
ageb_shp <- left_join(ageb_shp, pop_ageb)
ageb_shp <- left_join(ageb_shp, pop_ageb.fem)
ageb_shp <- left_join(ageb_shp, select(delitos_100k, -pop), by=c("CVEGEO"="cvegeo"))
ageb_shp <- left_join(ageb_shp, dist_apoyos)

#delitos_map lo dejaremos fuera porque es una serie de tiempo ---- 

ageb_shp %>% write_sf("intermediates/full_data.geojson")

################################################################################
