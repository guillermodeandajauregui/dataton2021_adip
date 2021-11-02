library(tidyverse)
library(archive)
library(vroom)
library(sf)
url <-"https://inegi.org.mx/contenidos/programas/ccpv/2020/datosabiertos/ageb_manzana/ageb_mza_urbana_09_cpv2020_csv.zip"


td<- tempdir()# Descarga del archivo temporaltf = tempfile(tmpdir=td, fileext=".zip")
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)# unzipunzip(tf, files="viviendas.csv", exdir=td, 
mi_file <- archive::archive(tf) %>% pull(path) %>% head(2) %>% tail(1)
x <- vroom::vroom(file = archive_read(archive = tf, file = mi_file))

unlink(td)
unlink(tf)
rm(td, tf)


x %>% colnames()

pop.ageb <- 
  x %>% 
  #filter(!grepl("Total", NOM_LOC)) %>%
  filter(grepl("Total AGEB", NOM_LOC)) %>% 
  mutate(CVEGEO=paste0(ENTIDAD,MUN,LOC, AGEB)) %>% 
  select(CVEGEO, POBTOT) %>% 
  group_by(CVEGEO) %>% 
  summarise(pop = sum(POBTOT))



ageb_shp   <- sf::st_read(dsn = "data/marco_geoestadistico2020/conjunto_de_datos/00a.shp") %>% filter(CVE_ENT=="09") %>% st_transform(crs = 4269) 
ageb_shp$CVEGEO %>% head() %>% str_length()

vroom::vroom_write(x = pop.ageb, file = "intermediates/pop_ageb_urbana.txt")

pop.ageb.fem <- 
  x %>% 
  #filter(!grepl("Total", NOM_LOC)) %>%
  filter(grepl("Total AGEB", NOM_LOC)) %>% 
  mutate(CVEGEO=paste0(ENTIDAD,MUN,LOC, AGEB)) %>% 
  mutate(POBFEM=as.numeric(POBFEM)) %>% 
  select(CVEGEO, POBFEM) %>% 
  #  select(1:15) %>% 
  #  filter(is.na(POBFEM))
    #drop_na()
  group_by(CVEGEO) %>% 
  summarise(popfem = sum(POBFEM))

vroom::vroom_write(x = pop.ageb.fem, file = "intermediates/pop_ageb_urbana_fem.txt")
