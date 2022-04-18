library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
datos <- read_excel("desastres.xlsx") %>% 
  filter(
    Country == "Guatemala",
    Year < 2020,
    Year > 1980,
    `Disaster Type` %in% c("Flood", "Drought", "Storm", "Landslide")
    ) %>% 
  mutate(
    Decada = 10*floor(as.numeric(Year)/10),
    Evento = recode(`Disaster Type`, Flood = "Inundación", Drought = "Sequía",
                    Storm = "Tormenta", Landslide = "Deslizamiento")
    ) %>% 
  count(Decada, Evento) %>% 
  group_by(Decada) %>% 
  mutate(Total = sum(n)) %>% 
  ungroup() %>% 
  mutate(
    pje = 100*n/Total,
    texto = paste0("<b>Década:</b> ", Decada, "<br>",
                   "<b>Evento:</b> ", Evento, "<br>",
                   "<b>Frecuencia:</b> ", n, "<br>",
                   "<b>Porcentaje:</b> ", formato2(pje))
    )

Encoding(datos$texto) <- "UTF-8"

original <- datos %>% 
  select("Década" = Decada, Evento, Frecuencia = n, Porcentaje = pje)

#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  plotlyOutput(outputId = "barra"),
  div(style = "height:25px"),
  
  splitLayout(
    cellWidths = c("35%", "35%", "30%"),
    materialSwitch(inputId = "id_tipo", label = "Ver Porcentaje", status = "success"),
    materialSwitch(inputId = "id_pila", label = "Barras Apiladas", status = "success"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
  )
)

#### SERVER ####
servidor <- function(input, output) {
  
  #Tipo de Grafico a visualizar
  botoncito <- reactive({input$id_tipo})
  botoncito2 <- reactive({input$id_pila})
  
  #Conjunto de datos a usar
  datos_re <- reactive({
    if (botoncito()) {
      rename(datos, Valor = pje)
    } else {
      rename(datos, Valor = n)
    }
  })
  
  #Barras Apiladas o no
  posicion <- reactive({
    if (botoncito2()) {
      "stack"
    } else {
      "dodge"
    }
  })
  
  #Nombre Eje Y
  titY <- reactive({
    if (botoncito()) {
      "Porcentaje"
      } else {
        "Cantidad de Eventos"
      }
  })
  
  #Saltos Eje Y
  ejey <- reactive({
    if (botoncito()) {
      seq(0, 100, 10)
    } else {
      
      if (botoncito2()) {
        Eje_OK(
          x = datos_re()$Total, 
          saltos = c(5, 10, 50, 100),
          cero = TRUE,
          optimo = 10
        )
      } else {
        Eje_OK(
          x = datos_re()$Valor, 
          saltos = c(5, 10, 50, 100),
          cero = TRUE,
          optimo = 10
        )
      }
      
    }
  })
  
  output$barra <- renderPlotly({
  
    #es mucho mejor un grafico de lineas
    z <- ggplot(data = datos_re()) +
      aes(x = Decada, fill = Evento, y = Valor, text = texto) +
      geom_bar(position = posicion(), stat = "identity") +
      scale_x_continuous(name = "Década", 
                         breaks = unique(datos_re()$Decada),
                         expand = c(0.01, 0.01)) +
      scale_y_continuous(name = titY(),
                         breaks = ejey(),
                         labels = formato(ejey()),
                         limits = range(ejey()),
                         expand = c(0.01, 0.01)) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.text = element_text(size = 8)
      )

    salida_ply <- estetica(
      grafico = z, 
      tit = "Eventos Extremos según Década", 
      subtit = "Eventos hidrometeorológicos extremos en Guatemala",
      posx = 1980
    )
  
    salida_ply
  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_Desastres.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
