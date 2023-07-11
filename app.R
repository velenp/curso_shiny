# Esto es una app de Shiny . Pueden correr la aplicación haciendo clic en
# el boton de arriba 'Run App' .
#
# Mas info:
#
#    http://shiny.rstudio.com/

#

##cargo los paquetes que voy a usar
####

library(shiny)
library(tidyverse)
library(tidyr)
library(lubridate)
library(plotly)
library(shinyWidgets)

##leo datos de mortalidad infantil del csv descargado de datos abiertos
##agrgregar q incluya descarga
##con if
##carpeta q se llame datos q la lea y si esta vacia lo descargue.

#datos <- read.csv("tasa-mortalidad-infantil-deis-1990-2021.csv", sep=";")
temp <- tempfile()
download.file(
  "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv",
  temp
)
data <- read.csv(temp)
unlink(temp)

###proceso los datos para graficar
bd <- data %>% pivot_longer(
  cols = !indice_tiempo,
  names_to = "prov",
  values_to = "TMI"
) %>%
  mutate(
    prov = str_sub(prov, 21, nchar(prov)),
    ano = year(ymd(indice_tiempo)),
    indice_tiempo = ymd(indice_tiempo)
  )

# Defino UI para mi aplicación
ui <- fluidPage(
  # Titulo
  titlePanel("Mortalidad infantil en Argentina"),
  
  # Sidebar con un a select input para seleccionar
  #la o la juris que quiero visualizar
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "prov",
      label = "Selecione una jurisdicción:",
      choices = unique(bd$prov),
      selected = "argentina",
      ## lo que quiero que muestre seleccionado pro defecto
      multiple = TRUE
    )
    
    
    #### una alternativa de select para usar. Puede encontrar mas opciones en 
    #https://shinyapps.dreamrs.fr/shinyWidgets/
    #prueben descomentando las siguientes 10 filas
    # ,
    # pickerInput(
    #   inputId = "prov2",
    #   label = "Jurisdicción:", 
    #   choices = unique(bd$prov),
    #   multiple = TRUE,
    #   selected = "argentina",
    #   options = list(
    #     `actions-box` = TRUE)
    # )
    
  ),
  
  # Muestro el gráfico
  mainPanel(plotlyOutput("Plot")))
)

# Defino server
server <- function(input, output) {
  output$Plot <- renderPlotly({
    # armo el grafico con plotly
    p = bd  %>%
      filter(prov %in% input$prov) %>%
      group_by(prov) %>%
      plot_ly(x = ~ indice_tiempo) %>%
      add_lines(y = ~ TMI,
                color = ~ factor(prov)) %>%
      layout(
        title = 'Título',
        xaxis = list(title = 'AÑO'),
        yaxis = list(title = 'Tasa de Mortalidad Infantil'),
        legend = list(title = list(text = '<b> Jurisdicción: </b>'))
      )
    p
  })
}

# Corro la application
shinyApp(ui = ui, server = server)
