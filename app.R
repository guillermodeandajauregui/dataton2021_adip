library(shiny)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(dplyr)
library(sf)
library(ggraph)
library(ggmap)

delitos <- read_tsv("intermediates/delitos_map_ageb.tsv")

delitos <- delitos %>% 
  mutate(mes_hecho = factor(mes_hecho, levels = c("Enero", "Febrero", "Marzo", 
  "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", 
  "Octubre", "Noviembre", "Diciembre")), 
  mes_hecho_num = as.numeric(mes_hecho), 
  fecha = as.Date(paste0(ano_hecho, "-", mes_hecho_num, "-01")))

getDelitosBetweenDates <- function(fecha_inicio, fecha_fin) {
  
  delitos_in_dates <- delitos %>% 
    filter(fecha >= fecha_inicio & fecha <= fecha_fin) %>%
    arrange(desc(fecha))

  return(delitos_in_dates)
}


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Focos de violencia contra mujeres en CDMX"),
  
  sidebarLayout(
    sidebarPanel(
      tags$label(h3('Filtrar delitos')),
      airDatepickerInput("fecha_inicio",
                         label = "Fecha inicial",
                         value = "2019-01-02",
                         maxDate = "2021-09-30",
                         minDate = "2019-01-02",
                         view = "months", #editing what the popup calendar shows when it opens
                         minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "mm-yyyy"
      ), 
      airDatepickerInput("fecha_fin",
                         label = "Fecha final",
                         value = "2021-09-30",
                         maxDate = "2021-09-30",
                         minDate = "2019-01-02",
                         view = "months", #editing what the popup calendar shows when it opens
                         minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "mm-yyyy"
      ),
      actionButton("submitbutton", "Enviar", class = "btn btn-primary")
    ),
    mainPanel(
      verbatimTextOutput('mensaje'),
      plotOutput(outputId = "geoPlot")
      
    )
  )
)

server <- function(input, output) {
  
    output$mensaje <- renderPrint({
      if (input$submitbutton > 0) {
        if(is.null(input$fecha_inicio) | is.null(input$fecha_fin)) {
          return("Fecha inválida") 
        }
      } else {
        return("Selecciona fechas para filtrar el conjunto de datos")
      }
    })
  
   getMap <- reactive({
    full_shp <- read_sf("intermediates/full_data.geojson") %>% 
      select(-delitos, -delitos_cien_k)
    
    delitos_in_fechas <- getDelitosBetweenDates(input$fecha_inicio, input$fecha_fin) %>%
      group_by(cvegeo) %>%
      tally(sort = T, name = "delitos")
    
    full_shp <- full_shp %>% left_join(delitos_in_fechas, 
                                       by = c("CVEGEO"="cvegeo")) %>%
      mutate(delitos_cien_fem = delitos/(popfem/100000), 
             delitos_cien_k = delitos/(pop/100000))
    
    print(head(full_shp %>% select(CVEGEO, delitos, delitos_cien_fem)))
    
    ggplot() +
      geom_sf(data =full_shp) +
      geom_sf(data = full_shp, 
               mapping = aes(fill = delitos_cien_fem), 
               lwd = 0.1) + 
       theme_minimal() + 
       scale_fill_viridis_c(option = "A", direction = -1, 
                            trans="log10", name = "Víctimas por 100 mil hab. mujeres",
                            limits = c(1, 1e7)) 
     
    
  })
   
  output$geoPlot <- renderPlot({
    if (input$submitbutton > 0) { 
      if(!is.null(input$fecha_inicio) & !is.null(input$fecha_fin)) {
        isolate(getMap()) 
      }
    }
  })
}

shinyApp(ui = ui, server = server)
