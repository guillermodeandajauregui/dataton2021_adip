library(vroom)
library(dplyr)
library(janitor)
library(ggraph)
library(ggmap)
library(sf)
library(lubridate)

path_ageb <- "~/DATA/marco_geoestadistico2020/conjunto_de_datos/00a.shp"
ageb_shp   <- sf::st_read(dsn = path_ageb) %>% filter(CVE_ENT=="09") %>% st_transform(crs = 4269)  #de https://www.inegi.org.mx/temas/mg/#Descargas


# victimas <- jsonlite::read_json("https://datos.cdmx.gob.mx/api/3/action/datastore_search?resource_id=d543a7b1-f8cb-439f-8a5c-e56c5479eeb5&limit=100000")
# 
# victimas <- 
#   victimas$result$records %>% #head %>% 
#   lapply(FUN = function(i){
#     
#     i %>%  as.data.frame() %>% as.tibble()  
#     
#   }) %>% bind_rows()
# 
# 
# victimas <- victimas %>% janitor::clean_names() %>%  filter(ano_hecho >= 2019) 

victimas <- vroom::vroom("victimas_completa_septiembre_2021.csv") %>% janitor::clean_names() %>%  filter(ano_hecho >= 2019) 

# victimas.masc %>% 
#   mutate(anum = year(fecha_hecho), 
#          month = month(fecha_hecho))
#   group_by(ano_inicio, mes_inicio) %>% 
#   tally()
  
victimas <- 
  victimas %>%
  mutate_at(vars(latitud, longitud), as.numeric) %>%   # coordinates must be numeric
  drop_na() %>% 
  sf::st_as_sf(
    coords = c("longitud", "latitud"),
    agr = "constant",
    crs = 4269,      
    stringsAsFactors = FALSE,
    remove = TRUE
  )

victimas_in_ageb <- 
  sf::st_join(victimas, ageb_shp, join = st_within) %>%
  select(ano_hecho, mes_hecho, fecha_hecho, delito, CVEGEO, delito, sexo) %>% 
  sf::st_drop_geometry()
  


victimas_in_ageb %>% 
  filter(sexo!="NA") %>% 
  mutate(fecha   = dmy(fecha_hecho)) %>% 
  mutate(mes_num = month(fecha),
         ano_num = year(fecha)
         ) %>% 
  group_by(CVEGEO, sexo, mes_hecho, ano_num, mes_num) %>% 
  tally() %>% 
  group_by(sexo, ano_num, mes_hecho, mes_num) %>% 
  summarise(n = sum(n)) %>% 
  mutate(mes = fct_reorder(.f = mes_hecho, .x = mes_num, .fun = sort)) %>% 
  mutate(ano_mes = paste0(ano_num, str_pad(mes_num, 2, "left", "0"))) %>% 
  #mutate(ano_mes = as.numeric(ano_mes)) %>% 
  ungroup() %>% 
  ggplot() + 
  aes(ano_mes, n, color = sexo, group=sexo) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  scale_color_manual(values = c("darkorange", "forestgreen")) + 
  ylab("Número de registros de víctimas") + 
  xlab("año-mes")


victimas_in_ageb %>% 
  filter(sexo!="NA") %>% 
  mutate(fecha   = dmy(fecha_hecho)) %>% 
  mutate(mes_num = month(fecha),
         ano_num = year(fecha)
  ) %>% 
  mutate(epi_week = epiweek(fecha)) %>% 
  mutate(semana   = paste0(ano_num, "-", str_pad(epi_week, width = 2, side = "left", pad = "0"))) %>% 
  group_by(sexo, semana) %>% 
  tally() %>% 
  ungroup() %>% 
  ggplot() + 
  aes(semana, n, color = sexo, group=sexo) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  scale_color_manual(values = c("darkorange", "forestgreen")) + 
  ylab("Número de registros de víctimas") + 
  xlab("semana") + 
  geom_vline(xintercept = "2020-16", linetype=3) + 
  theme(text = element_text(size=rel(4)),
        axis.text.x = element_text(angle=90), 
        legend.position = "none"
  )



delitos_fem <- 
  victimas_in_ageb %>% 
  filter(sexo=="Femenino") %>% 
  mutate(fecha   = dmy(fecha_hecho)) %>% 
  mutate(mes_num = month(fecha),
         ano_num = year(fecha)
  ) %>% 
  mutate(epi_week = epiweek(fecha)) %>% 
  mutate(semana   = paste0(ano_num, "-", str_pad(epi_week, width = 2, side = "left", pad = "0"))) %>% 
  group_by(semana, delito) %>% 
  tally() %>% pull(delito) %>% unique() %>% sort


c("meco", "caca", "caca fina", "canela", "canela fina", "caca seca") %>% grep(pattern = "caca|fina", x = ., value = T)

delitos_fem.exp <- delitos_fem %>% grep(pattern = "FAMILIAR|VIOLACION|FEMINICIDIO|SEXUAL", x = ., value = T)
  

victimas_in_ageb %>% 
  filter(sexo=="Femenino") %>% 
  mutate(fecha   = dmy(fecha_hecho)) %>% 
  mutate(mes_num = month(fecha),
         ano_num = year(fecha)
  ) %>% 
  mutate(epi_week = epiweek(fecha)) %>% 
  mutate(semana   = paste0(ano_num, "-", str_pad(epi_week, width = 2, side = "left", pad = "0"))) %>% 
  filter(delito%in%delitos_fem.exp) %>% 
  group_by(semana) %>% 
  tally() %>% 
  ungroup() %>% 
  ggplot() + 
  aes(semana, n, group=1) + 
  geom_line(color="goldenrod") + 
  geom_point(color="goldenrod") + 
  theme_minimal() + 
  ylab("Num. registros víctimas fem, delitos FAMILIAR|VIOLACION|FEMINICIDIO|SEXUAL") + 
  xlab("semana") + 
  geom_vline(xintercept = "2020-16", linetype=3) + 
  theme(text = element_text(size=rel(4)),
        axis.text.x = element_text(angle=90)
        )


victimas_in_ageb %>% 
  #filter(sexo=="Femenino") %>% 
  mutate(fecha   = dmy(fecha_hecho)) %>% 
  mutate(mes_num = month(fecha),
         ano_num = year(fecha)
  ) %>% 
  mutate(epi_week = epiweek(fecha)) %>% 
  mutate(semana   = paste0(ano_num, "-", str_pad(epi_week, width = 2, side = "left", pad = "0"))) %>% 
  filter(delito%in%delitos_fem.exp) %>% 
  group_by(semana, sexo) %>% 
  tally() %>% 
  ungroup() %>% 
  ggplot() + 
  aes(semana, n, color = sexo, group=sexo) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  ylab("Num. registros víctimas fem, delitos FAMILIAR|VIOLACION|FEMINICIDIO|SEXUAL") + 
  xlab("semana") + 
  scale_color_manual(values = c("darkorange", "forestgreen")) + 
  geom_vline(xintercept = "2020-16", linetype=3) + 
  theme(text = element_text(size=rel(4)),
        axis.text.x = element_text(angle=90),
        legend.position = "none"
  )
