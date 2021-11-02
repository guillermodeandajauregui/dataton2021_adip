#mapear los sitios de atencion
#sacar su distancia en calles a los centroides de lo que sea
#correlacionar con numero de llamadas 911 y mujeres
#correlacionar con desigualdad
###
#poner en un shiny
#un mapita 
#un 
################################################################################
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggmap)
library(sf)
library(sfnetworks)
#read shapefile
rnc_shapes <- sf::st_read(dsn = "~/data/Red_Nacional_de_Caminos_2019/conjunto_de_datos/red_vial.shp") %>% st_transform(crs = 4269) #de http://nube.imt.mx/index.php/s/8cG6S5KSH0Yz6JV
ageb_shp   <- sf::st_read(dsn = "~/data/00a.shp") %>% filter(CVE_ENT=="09") %>% st_transform(crs = 4269)  #de https://www.inegi.org.mx/temas/mg/#Descargas


rnc_shapes <- 
  st_filter(rnc_shapes, 
            st_as_sfc(st_bbox(ageb_shp)), 
            .predicate = st_within)

gc()

mujeres <- read_sf("data/servicios_de_atencion_a_violencia_contra_las_mujeres/servicios_de_atencion_a_violencia_contra_las_mujeres.shp") %>% st_transform(crs = 4269) # de https://datos.cdmx.gob.mx/dataset/servicios-atencion-violencia-mujeres-durante-contingencia-covid19

mujeres <- 
  st_filter(mujeres, 
            st_as_sfc(st_bbox(ageb_shp)), 
            .predicate = st_within)

gc()
################################################################################
#fase uno: red de caminos

rnc_shapes %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  #group_by(TIPO_VIAL) %>%
  group_by(VELOCIDAD) %>% 
  tally(sort = T)

ggplot() + 
  geom_sf(data = ageb_shp) +
  geom_sf(data = filter(rnc_shapes, !(CARRILES%in%c("N/A", "0"))), mapping = aes(color = TIPO_VIAL)) + 
  theme_minimal()


#hacerla red

rnc_shapes <- rnc_shapes %>% as_sfnetwork() 


ggplot() + 
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(activate(rnc_shapes, "edges")), 
          mapping = aes(color = TIPO_VIAL)) + 
  theme_minimal()

################################################################################

#vamos a sacar la distancia de los lugares de atencion a victimas



ggplot() + 
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(activate(rnc_shapes, "edges")), 
          #mapping = aes(color = TIPO_VIAL)
  ) + 
  geom_sf(data = mujeres, color = "red") + 
  theme_minimal()

#voy a sacar los centroides de cada ageb

centroides.ageb <- 
  ageb_shp %>% 
  st_centroid() %>% 
  st_as_sf() %>% 
  select(CVEGEO)

#ahora voy a calcular la distancia del centroide de cada ageb, a su centro mujeres 
#y me quedo el minimo 


mujeres.geo <- st_geometry(mujeres)

mujeres.geo %>% length()
mujeres.geo %>% unique %>% length()
agebs.geo   <- st_geometry(ageb_shp)  

components(rnc_shapes)
new_net <- 
  rnc_shapes %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>% 
  st_network_blend(c(st_geometry(mujeres), st_geometry(centroides.ageb)))

cost_matrix = st_network_cost(new_net, 
                              from = agebs.geo, 
                              to = mujeres.geo, 
                              weights = "LONGITUD")

nrow(cost_matrix)
apply(cost_matrix, 1, function(x) x[which(x == min(x))[1]])

ageb_shp <- 
  ageb_shp %>% 
  mutate(distancia_apoyo_mujeres = apply(cost_matrix, 1, function(x) x[which(x == min(x))[1]])) %>% 
  mutate(distancia_apoyo_mujeres = distancia_apoyo_mujeres + 1)

ggplot() +
  geom_sf(data =ageb_shp,  mapping = aes(fill = distancia_apoyo_mujeres), lwd=0.1) + 
  geom_sf(data = st_as_sf(activate(rnc_shapes, "edges")), 
          #mapping = aes(color = TIPO_VIAL)
  ) + 
  geom_sf(data = mujeres, color = "red") + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "A", trans="log10") # + 


ageb_shp %>% 
  as_tibble() %>% 
  select(CVEGEO, distancia_apoyo_mujeres) %>% 
  vroom::vroom_write("intermediates/distancias_apoyoMujeres.txt")
