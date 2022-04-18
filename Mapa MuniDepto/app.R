library(sf)
library(gplots)
library(leaflet)
library(viridis)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(shiny)
library(tidyverse)
library(mapview)
source("fuente.R", encoding = "UTF-8")

#library(leaflet.extras2) #addTimeslider solo sirve para puntos creo
#https://rstudio.github.io/leaflet/shiny.html

# original <- read_csv("HOGAR_BDP.csv") %>%
#   select(DEPARTAMENTO,MUNICIPIO,PCH15)
# 
# datos1 <- original %>% group_by(DEPARTAMENTO,MUNICIPIO) %>%
#   summarize(Valor = mean(PCH15 == 1, na.rm = TRUE)*100)
# 
# datos2 <- original %>% group_by(DEPARTAMENTO) %>%
#   summarize(Valor = mean(PCH15 == 1, na.rm = TRUE)*100)
# 
# save(datos1, file = "remeses.hogares.muni.RData")
# save(datos2, file = "remeses.hogares.depto.RData")

#### DATOS ####
load("poligonos.RData")
load("remeses.hogares.muni.RData")
load("remeses.hogares.depto.RData")
belice <- readRDS("gadm36_BLZ_0_sf.rds")

Ncol <- 5
paleta <- RColorBrewer::brewer.pal(Ncol, "Greens")

#Municipios
guate1 <- guates[[2]] %>% 
  left_join(datos1, by = c("INE" = "MUNICIPIO")) %>% 
  mutate(
    grupos = cut(Valor, breaks = seq(0, 40, length.out = Ncol + 1), right = FALSE),
    color = paleta[as.numeric(grupos)]
  )

#texto del pop-up
txt1 <- as.list(
  paste0(
    "<b>Departamento:</b> ", guate1$Departamento, 
    "<br><b>Municipio:</b> ", guate1$Nombre, 
    "<br><b>Hogares con remesas:</b> ", round(guate1$Valor, 2), " %")
)

#Departamentos
guate2 <- guates[[1]] %>% 
  left_join(datos2, by = c("INE" = "DEPARTAMENTO")) %>% 
  mutate(
    grupos = cut(Valor, breaks = seq(0, 20, length.out = Ncol + 1), right = FALSE),
    color = paleta[as.numeric(grupos)]
  )

#texto del pop-up
txt2 <- as.list(
  paste0(
    "<b>Departamento:</b> ", guate2$Nombre, 
    "<br><b>Hogares con remesas:</b> ", formato2(guate2$Valor), " %")
  )


#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  h1(strong("Hogares con remesas"), style = "font-size:22px;"),
  #titlePanel("Edad Promedio en Guatemala"),
  leafletOutput(outputId = "mapa"),
  div(style = "height:25px"),
  
  splitLayout(
    cellWidths = c("35%", "35%", "30%"),
    materialSwitch(inputId = "id_tipo", label = "Ver Municipios", status = "success"),
    downloadButton(outputId = "id_foto", label = "Descargar Mapa"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
  )
)

#### SERVER ####
servidor <- function(input, output) {
  
  #Tipo de Grafico a visualizar
  botoncito <- reactive({input$id_tipo})
  
  #Conjunto de datos a usar
  guate <- reactive({if (botoncito()) {guate1} else {guate2}})

  #Textos de pop-up a usar
  textos <- reactive({if (botoncito()) {txt1} else {txt2}})

  #Trampa para descargar imagen del mapa
  #no se bien que funcion cumplen los objetos/argumentos diego y benja
  #fuente: https://stackoverflow.com/questions/44259716/how-to-save-a-leaflet-map-in-shiny
  
  diego <- reactiveValues(benja = 0)
  
  output$mapa <- renderLeaflet({

    diego$benja <- leaflet() %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addPolygons(
        data = guate(),
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~color,
        label = ~lapply(textos(), HTML)
      ) %>%
      addPolylines(
        data = guate2,
        color = "black",
        weight = 1
      ) %>%
      addPolygons(
        data = belice,
        color = "black",
        weight = 1,
        fillOpacity = 0.3,
        fillColor = "grey",
        label = "Diferendo territorial, insular y marítimo pendiente de resolver"
      ) %>% 
      addLegend(
        title = "Porcentaje",
        colors = paleta,
        labels = levels(guate()$grupos)
        ) %>% 
      leafem::addLogo(logo) %>% 
      setView(-90.34, 15.81, zoom = 7)

  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_Mapa.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
  output$id_foto <- downloadHandler(
    filename = "Guate_en_Datos_Mapa.png",
    content = function(file) {mapshot(diego$benja, file = file)}
  )

}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
