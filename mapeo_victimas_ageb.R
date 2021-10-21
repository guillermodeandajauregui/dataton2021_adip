library(vroom)
library(dplyr)
library(janitor)
library(ggraph)
library(ggmap)
library(sf)

victimas <- vroom::vroom("data/victimas_en_carpetas_de_investigacion_fgj/victimas_completa_septiembre_2021.csv") %>%
  janitor::clean_names()
victimas <- victimas %>% filter(sexo == "Femenino" & ano_hecho >= 2019) 

delitos <- victimas %>% group_by(delito) %>% tally(sort = T)
### Inspección visual indica que todos los delitos con más de 200 víctimas registraras 
### podrían ser relevantes, salvo PERDIDA DE LA VIDA POR SUICIDIO
delitos <- delitos %>% 
  filter(n >= 200 & delito != "PERDIDA DE LA VIDA POR SUICIDIO") %>%
  pull(delito)

victimas <- victimas %>% filter(delito %in% delitos & 
                                  !is.na(longitud) &
                                  !is.na(latitud))

ageb_shp <- sf::st_read(dsn = "data/marco_geoestadistico2020/conjunto_de_datos/00a.shp") %>% 
  janitor::clean_names() %>%
  filter(cve_ent == "09") %>%
  st_transform(crs = 4269) 

victimas <- victimas %>%
  mutate_at(vars(latitud, longitud), as.numeric) %>%   # coordinates must be numeric
  sf::st_as_sf(
    coords = c("longitud", "latitud"),
    agr = "constant",
    crs = 4269,      
    stringsAsFactors = FALSE,
    remove = TRUE
  )

victimas_in_ageb <- sf::st_join(victimas, ageb_shp, join = st_within) %>%
  select(ano_hecho, mes_hecho, delito, cvegeo, delito) %>% 
  sf::st_drop_geometry()
victimas_in_ageb %>% vroom::vroom_write("intermediates/delitos_map_ageb.tsv")

victimas_by_ageb <- victimas_in_ageb %>% 
  group_by(cvegeo) %>%
  tally(sort = T, name = "delitos")

pop_ageb <- vroom::vroom("intermediates/pop_ageb_urbana.txt") %>%
  janitor::clean_names()

delitos_pop_ageb <- pop_ageb %>% left_join(victimas_by_ageb, by = "cvegeo") %>%
  mutate(delitos_cien_k = delitos/(pop/100000))

delitos_pop_ageb %>% vroom::vroom_write("intermediates/delitos_100k_ageb.tsv")

delitos_pop_ageb <- delitos_pop_ageb %>% inner_join(
  ageb_shp %>% select(cvegeo, geometry),
  by = "cvegeo")

ggplot() +
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(delitos_pop_ageb), 
          mapping = aes(fill = delitos_cien_k), 
          lwd = 0.1) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "A", direction = -1, 
                       trans="log10", name = "Víctimas por 100 mil hab.") 
