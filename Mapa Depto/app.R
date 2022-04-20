library(sf)
library(leaflet)
library(readxl)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(janitor)
library(shiny)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

formato2 <- function(x) {
  trimws(format(round(x, 0), big.mark = ",", decimal.mark = ".", 
                scientific = FALSE, nsmall = 0))
}

#### DATOS ####
load("poligonos.RData")
belice <- readRDS("gadm36_BLZ_0_sf.rds")
Ncol <- 10 #cantidad de colores para el heatmap
paleta <- mapview:::col2Hex(viridis::viridis(Ncol))

original <- read_excel("area.xls", sheet = 4) %>% 
  slice(-c(1:4)) %>% 
  slice(-c(2,25))


cols.num <- c("...3","...4","...5","...6","...7","...8","...9","...10","...11","...12",
              "...13","...14","...15","...16","...17","...18",
              "...19","...20","...21","...22","...23","...24","...25","...26","...27",
              "...28","...29","...30","...31","...32","...33","...34","...35","...36",
              "...37","...38","...39","...40","...41","...42","...43","...44","...45",
              "...46","...47","...48","...49","...50","...51","...52","...53")
original[cols.num] <- sapply(original[cols.num],as.numeric)

datos <- original %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  replace(is.na(.), 0) %>%
  mutate(Valor = na_2+na_3+na_4+na_5+na_6+na_7+na_8+na_9+na_10+na_11+na_12+
           na_13+na_14+na_15+na_16+na_17+na_18+na_19,
         codigo = as.numeric(codigo))
  
  
guate1 <- guates[[1]] %>% 
  left_join(datos, by = c("INE" = "codigo")) %>%
  mutate(
    decil=ntile(Valor,Ncol),
    grupos = cut(decil, breaks = c(1:11), right = FALSE),
    color = paleta[as.numeric(grupos)]
  )

#texto del pop-up
txt1 <- as.list(
  paste0(
    "<b>Departamento:</b> ", guate1$Nombre, 
    "<br><b>Cobertura forestal:</b> ", formato2(guate1$Valor), " ha"
))

#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  h1(strong("Cobertura forestal - 2020"), style = "font-size:22px;"),
  #titlePanel("Edad Promedio en Guatemala"),
  leafletOutput(outputId = "mapa"),
  div(style = "height:25px"),
  p("Fuente: INE (2022)", style = "font-size:12px;"),
  splitLayout(
    cellWidths = c("40%","30%", "30%"),
    p("UVG/GuateenDatos/Bosques-CCBY", style = "font-size:12px;"),
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

    diego$benja <- leaflet() %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addPolygons(
        data = guate1,
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~color,
        label = ~lapply(txt1, HTML)
      ) %>%
      addPolylines(
        data = guate1,
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
        title = "Decil",
        colors = paleta,
        labels = c(1:10)
        ) %>% 
      leafem::addLogo(logo) %>% 
      setView(-90.34, 15.81, zoom = 7)

  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_CobFor.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
  output$id_foto <- downloadHandler(
    filename = "Guate_en_Datos_CobFor.png",
    content = function(file) {mapshot(diego$benja, file = file)}
  )
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
