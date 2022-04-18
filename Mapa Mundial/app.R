library(sf)
library(stringr)
library(leaflet)
library(readxl)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(shiny)
library(tidyverse)
library(mapview)
source("fuente.R", encoding = "UTF-8")

# library(sf)
# library(dplyr)
# library(rmapshaper)
# mapa <- read_sf(dsn = "GADM", layer = "gadm36_0")
# mapa2 <- filter(mapa, !NAME_0 %in% c("Fiji", "Russia"))
# mapa3 <- filter(mapa, NAME_0 %in% c("Fiji", "Russia"))
# #z <- st_simplify(mapa2, dTolerance = 1000)
# save(z, file = "mapitasimple.RData")
# load("mapitasimple.RData")
# rusia_fidji <- ms_simplify(mapa3, weighting = 0.7)
# definitivo <- bind_rows(z, rusia_fidji)
# save(definitivo, file = "mapa.RData")

#### DATOS ####
load("mapa.RData")
original <- read_excel("estadoderecho.xlsx", sheet = "WJP ROL Index 2021 Scores")

datos <- original %>% 
  slice(1:4) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("Pais") %>%
  slice(-1) %>%
  rename(Region = 3, Ingreso = 4, Valor = 5) %>%
  mutate(
    Valor = as.numeric(Valor),
    grupos = cut(Valor, breaks = seq(0.2,0.9,0.1), right = FALSE),
    Ingreso = recode(Ingreso, "High" = "Alto", "Low" = "Bajo", 
                     "Lower middle" = "Medio-bajo", "Upper middle" = "Medio-alto"),
    Pais = recode(Pais,
                  "Egypt, Arab Rep." = "Egypt",
                  "Hong Kong SAR, China" = "Hong Kong",
                  "Iran, Islamic Rep." = "Iran",
                  "Russian Federation" = "Russia",
                  "The Bahamas" = "Bahamas",
                  "The Gambia" = "Gambia",
                  "Venezuela, RB" = "Venezuela",
                  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                  "Congo, Rep." = "Republic of Congo",
                  "Cote d'Ivoire" = "Côte d'Ivoire",
                  "Korea, Rep." = "South Korea",
                  "Kyrgyz Republic" = "Kyrgyzstan",
                  #"North Macedonia" = "XXX",
                  "Slovak Republic" = "Slovakia",
                  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
                  "St. Lucia" = "Saint Lucia",
                  "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines"
                  )
  ) %>% 
  arrange(Pais)

original <- datos %>% select(1, 3:5)
#Encoding(datos$texto) <- "UTF-8"
paises <- sort(unique(datos$Pais))

Ncol <- length(levels(datos$grupos)) #cantidad de colores distintos de la paleta (ver cut y breaks)
paleta <- mapview:::col2Hex(viridis::viridis(Ncol))

mapa <- definitivo %>% 
  slice(-c(219, 235, 250)) %>% #Tokelau, Vaticano, Spratly Islands
  left_join(datos, by = c("NAME_0" = "Pais")) %>% 
  mutate(
    color = paleta[as.numeric(grupos)],
    texto = paste0("<b>País:</b> ", NAME_0, "<br>",
                   "<b>Rule of Law Index:</b> ", formato2(Valor), "<br>",
                   "<b>Nivel de ingresos:</b> ", Ingreso)
  )

#setdiff(mapa$NAME_0,datos$Pais)
setdiff(datos$Pais, mapa$NAME_0)

#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  h1(strong("Índice de Estado de Derecho"), style = "font-size:22px;"),
  #titlePanel("Edad Promedio en Guatemala"),
  leafletOutput(outputId = "mapa"),
  div(style = "height:25px"),
  
  splitLayout(
    cellWidths = c("40%", "30%", "30%"),
    p("UVG/GuateenDatos/EstadodeDerecho-CCBY", style = "font-size:12px;"),
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
        data = mapa,
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        fillColor = ~color,
        label = ~lapply(as.list(mapa$texto), HTML)
      ) %>%
      addLegend(
        colors = paleta,
        labels = levels(mapa$grupos)
        ) %>% 
      leafem::addLogo(logo) %>% 
      setView(-90.34, 15.81, zoom = 5)
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
