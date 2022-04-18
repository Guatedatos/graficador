library(sf)
library(leaflet)
library(readxl)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(shiny)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
load("poligonos.RData")
belice <- readRDS("gadm36_BLZ_0_sf.rds")
datos <- read_excel(path = "FUNDESA - ICL 2020 Database (301020).xlsx", 
                    skip = 8, n_max = 340) %>% select(3:6)

original <- datos %>% select(-2)
paleta <- c("#FE4042", "#F4A216", "#F5D911", "#83DC16", "#579AC7")

guate2 <- guates[[2]] %>% 
  left_join(datos, by = c("INE" = "CÓDIGO MUNICIPIO")) %>% 
  mutate(
    grupos = cut(`ICL 2020`, breaks = seq(0, 100, 20), 
                 right = FALSE, labels = c("0 a 20", "20 a 40", 
                                           "40 a 60", "60 a 80", "80 a 100")),
    color = paleta[as.numeric(grupos)]
  )

#texto del pop-up
txt2 <- as.list(
  paste0(
    "<b>Departamento:</b> ", guate2$Departamento, 
    "<br><b>Municipio:</b> ", guate2$Nombre, 
    "<br><b>ICL:</b> ", formato2(guate2$`ICL 2020`))
)

#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  h1(strong("Índice de Competitividad Local"), style = "font-size:22px;"),
  #titlePanel("Edad Promedio en Guatemala"),
  leafletOutput(outputId = "mapa"),
  div(style = "height:25px"),
  # p("Fuente: Gapminder(2020)", style = "font-size:12px;"),
  # p("UVG/GuateenDatos/ProductoInternoBruto(PIB)-CCBY", style = "font-size:12px;"),
  
  splitLayout(
    cellWidths = c("40%", "30%", "30%"),
    p("Fuente: Gapminder(2020)", br(), 
      "UVG/GuateenDatos/ProductoInternoBruto(PIB)-CCBY", style = "font-size:12px;"),
    downloadButton(outputId = "id_foto", label = "Descargar Mapa"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
  )
)

#### SERVER ####
servidor <- function(input, output) {
  
  #Trampa para descargar imagen del mapa
  #no se bien que funcion cumplen los objetos/argumentos diego y benja
  #fuente: https://stackoverflow.com/questions/44259716/how-to-save-a-leaflet-map-in-shiny
  
  diego <- reactiveValues(benja = 0)
  
  output$mapa <- renderLeaflet({

    # Stamen.TonerBackground
    # Stamen.TerrainBackground
    # CartoDB.VoyagerNoLabels
    # CartoDB.DarkMatterNoLabels
    
    diego$benja <- leaflet() %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addPolygons(
        data = guate2,
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~color,
        label = ~lapply(txt2, HTML)
      ) %>%
      addPolylines(
        data = guates[[1]],
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
        colors = paleta,
        labels = levels(guate2$grupos)
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
