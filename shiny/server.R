library(readr)
library(dplyr)
library(sf)
library(ggraph)
library(ggmap)
library(cowplot)

full_shp <- read_sf("data/full_data.geojson") %>% 
  select(-delitos, -delitos_cien_k) %>%
  left_join(read_tsv("data/delitos_pre-covid_ageb.tsv"), 
            by = c("CVEGEO" = "cvegeo")) %>%
  left_join(read_tsv("data/delitos_pos-covid_ageb.tsv"), 
            by = c("CVEGEO" = "cvegeo"), 
            suffix = c("_pre", "_pos"))  %>%
  mutate(delitos_pre_100_pop = delitos_pre/(pop/100000),
         delitos_pos_100_pop = delitos_pos/(pop/100000),
         delitos_pre_100_fem = delitos_pre/(popfem/100000),
         delitos_pos_100_fem = delitos_pos/(popfem/100000))
  
varsLista <- c("Delitos pre 'No estás Sola'" = "delitos_pre",
               "Delitos pos 'No estás Sola'" = "delitos_pos",
               "Delitos pre 'No estás Sola' \npor 100 mil habitantes" = "delitos_pre_100_pop",
               "Delitos pos 'No estás Sola' \npor 100 mil habitantes" = "delitos_pos_100_pop",
               "Delitos pre 'No estás Sola' \npor 100 mil mujeres" = "delitos_pre_100_fem",
               "Delitos pos 'No estás Sola' \npor 100 mil mujeres" = "delitos_pos_100_fem",
               "Distancia AGEB - Centro de atención" = "distancia_apoyo_mujeres",
               "Índice de marginación" = "IM_2020", 
               "Población" = "pop", 
               "Población de mujeres" = "popfem")

function(input, output) {
  
  output$geoPlot <- renderPlot({
    
    plot1 <- ggplot() +
      geom_sf(data = full_shp) +
      geom_sf(data = full_shp,
              mapping = aes_string(fill = input$varA),
              lwd = 0.1) +
      theme_minimal() +
      scale_fill_viridis_c(option = "G", direction = -1,
                           trans="log10", name = names(varsLista)[varsLista == input$varA])

    plot2 <- ggplot() +
      geom_sf(data = full_shp) +
      geom_sf(data = full_shp,
              mapping = aes_string(fill = input$varB),
              lwd = 0.1) +
      theme_minimal() + 
       scale_fill_viridis_c(option = "A", direction = -1, 
                            trans="log10", name = names(varsLista)[varsLista == input$varB]) 
       
    plot_grid(plot1, plot2, labels = "AUTO")
    
  })
  
  output$corrPlot <- renderPlot({
    ggplot() +
      geom_point(data =full_shp, aes_string(x = input$varA, 
                                            y = input$varB)) +
      theme_minimal() +
      xlab(names(varsLista)[varsLista == input$varA]) +
      ylab(names(varsLista)[varsLista == input$varB]) 
  })
  
}
