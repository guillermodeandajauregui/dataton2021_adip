library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)

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

fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Distribución territorial de la violencia contra las mujeres: 
  oportunidades para optimizar cobertura."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("varA", "Variable A", 
                  choices = varsLista),
      selectInput("varB", "Variable B", 
                  choices = varsLista),
      width = 3
    ),
    
    mainPanel(
      plotOutput(outputId = "geoPlot"),
      tags$label(h3('Correlación entre variables')),
      plotOutput(outputId = "corrPlot", width = "90%",),
      width = 9
    )
  )
)
