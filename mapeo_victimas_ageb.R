library(vroom)
library(dplyr)
library(janitor)
library(ggraph)
library(ggmap)
library(sf)
library(cowplot)

victimas <- vroom::vroom("data/victimas_en_carpetas_de_investigacion_fgj/victimas_completa_septiembre_2021.csv") %>%
  janitor::clean_names()

# Filtrar delitos
victimas <- victimas %>% 
 filter(sexo == "Femenino" & ano_hecho >= 2019 & 
        !is.na(longitud) & !is.na(latitud) &
        (grepl(pattern = "FAMILIAR|VIOLACION|FEMINICIDIO|SEXUAL", delito) |
        grepl(pattern = "FAMILIAR|VIOLACION|FEMINICIDIO|SEXUAL", categoria))) %>%
 mutate(mes_hecho = factor(mes_hecho, levels = c("Enero", "Febrero", "Marzo", 
                                                  "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", 
                                                  "Octubre", "Noviembre", "Diciembre")), 
         mes_hecho_num = as.numeric(mes_hecho), 
         fecha = as.Date(paste0(ano_hecho, "-", mes_hecho_num, "-01"))) %>%
  mutate_at(vars(latitud, longitud), as.numeric) %>%   
  sf::st_as_sf(
    coords = c("longitud", "latitud"),
    agr = "constant",
    crs = 4269,      
    stringsAsFactors = FALSE,
    remove = TRUE
  )

## Mapear AGEBs
ageb_shp <- sf::st_read(dsn = "data/marco_geoestadistico2020/conjunto_de_datos/00a.shp") %>% 
  janitor::clean_names() %>%
  filter(cve_ent == "09") %>%
  st_transform(crs = 4269) 

victimas <- victimas %>% 
  sf::st_join(ageb_shp, join = st_within) %>%
  select(fecha, delito, cvegeo) %>% 
  sf::st_drop_geometry()

victimas_pos_covid <- victimas %>% 
  filter(fecha >= "2020-04-01" & fecha < "2021-04-01")

victimas_pre_covid <- victimas %>% 
  filter(fecha >= "2019-04-01" & fecha < "2020-04-01")

victimas_pos_covid %>% vroom::vroom_write("intermediates/delitos_pos-covid.tsv")
victimas_pre_covid %>% vroom::vroom_write("intermediates/delitos_pre-covid.tsv")

victimas_pos_covid_by_ageb <- victimas_pos_covid %>% 
  group_by(cvegeo) %>%
  tally(sort = T, name = "delitos")

victimas_pre_covid_by_ageb <- victimas_pre_covid %>% 
  group_by(cvegeo) %>%
  tally(sort = T, name = "delitos")

victimas_pos_covid_by_ageb %>% vroom::vroom_write("intermediates/delitos_pos-covid_ageb.tsv")
victimas_pre_covid_by_ageb %>% vroom::vroom_write("intermediates/delitos_pre-covid_ageb.tsv")


### Incidencia por 100mil hab

pop <- vroom::vroom("intermediates/pop_ageb_urbana.txt") %>%
  janitor::clean_names()

victimas_pre_covid_by_ageb <- victimas_pre_covid_by_ageb %>% 
  inner_join(pop, by = "cvegeo") %>%
  mutate(delitos_100k = delitos/(pop/100000)) %>%
  inner_join(
    ageb_shp %>% select(cvegeo, geometry),
  by = "cvegeo")

victimas_pos_covid_by_ageb <- victimas_pos_covid_by_ageb %>% 
  inner_join(pop, by = "cvegeo") %>%
  mutate(delitos_100k = delitos/(pop/100000)) %>%
  inner_join(
    ageb_shp %>% select(cvegeo, geometry),
    by = "cvegeo")

ppre <- ggplot() +
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(victimas_pre_covid_by_ageb), 
          mapping = aes(fill = delitos_100k), 
          lwd = 0.1) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "A", direction = -1, 
                       trans="log10", name = "Víctimas por 100 mil hab.") 

ppos <- ggplot() +
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(victimas_pos_covid_by_ageb), 
          mapping = aes(fill = delitos_100k), 
          lwd = 0.1) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "A", direction = -1, 
                       trans="log10", name = "Víctimas por 100 mil hab.") 

plot_grid(ppre, ppos, labels = c("Pre-programa 'No estás sola' (12 meses)", 
                                 "Pos-programa 'No estás sola' (12 meses)"))


victimas <- victimas_pre_covid_by_ageb %>% 
  select(cvegeo, delitos_100k, delitos, geometry) %>%
  full_join(victimas_pos_covid_by_ageb %>% 
                select(cvegeo, delitos_100k, delitos, geometry),
              by = c("cvegeo", "geometry"), suffix = c("_pre", "_pos"),)

victimas[is.na(victimas)] = 0

victimas <- victimas %>% 
  mutate(delta = delitos_100k_pos - delitos_100k_pre)

ggplot() +
  geom_sf(data = ageb_shp) +
  geom_sf(data = st_as_sf(victimas), 
          mapping = aes(fill = delta), 
          lwd = 0.1) + 
  theme_minimal() + 
  scale_fill_gradient2(low ="blue", high = "red",
                       trans="pseudo_log") +
  theme(legend.position = "none") 

