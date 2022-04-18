library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
original <- read_excel("PIB produccion desde 2005 long.xlsx")

datos <- original %>% 
  filter(Actividad != "PIB") %>% 
  select(Anio = 2, Actividad, Valor) %>% 
  arrange(desc(Anio), Valor) %>% 
  mutate(
    Valor = Valor/1000,
    texto = paste0("<b>Actividad:</b> ", Actividad, "<br>",
                   "<b>Año:</b> ", Anio, "<br>",
                   "<b>Valor:</b> ", formato2(Valor) #" &times; 10<sup>9</sup>"
                   ),
    Actividad = str_wrap(Actividad, 15),
    Actividad = factor(Actividad, levels = unique(Actividad))
    ) %>% 
  group_by(Anio) %>%
  mutate(Total = sum(Valor)) %>%
  ungroup() %>%
  mutate(Pje = 100*Valor/Total)

Encoding(datos$texto) <- "UTF-8"

#### INTERFAZ ####
interfaz <- fluidPage(
  
  title = "Guatemala en Datos",
  plotlyOutput(outputId = "area"),
  div(style = "height:25px"),
  
  splitLayout(
    cellWidths = c("75%", "25%"),
    materialSwitch(inputId = "id_tipo", label = "Ver Porcentaje", status = "success"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
  )
)

#### SERVER ####
servidor <- function(input, output) {
  
  #Tipo de Grafico a visualizar
  botoncito <- reactive({input$id_tipo})
  
  #Conjunto de datos a usar
  datos_re <- reactive({
    if (botoncito()) {
      select(datos, Valor = Pje, Actividad, Anio, texto, Total)
    } else {
      datos
    }
  })
  
  #Nombre Eje Y
  titY <- reactive({
    if (botoncito()) {
      "Porcentaje"
      } else {
        "Miles de millones de quetzales de 2013"
      }
  })
  
  #Saltos Eje Y
  ejey <- reactive({
    if (botoncito()) {
      seq(0, 100, 10)
    } else {
      Eje_OK(
        x = datos_re()$Total, 
        saltos = c(5, 10, 50, 100),
        cero = !botoncito())
    }
  })
  
  #Saltos Eje X
  ejex <- reactive({Eje_OK(x = datos_re()$Anio, saltos = 1, cero = FALSE)})
  
  output$area <- renderPlotly({
  
    z <- ggplot(data = datos_re()) +
      aes(x = Anio, y = Valor, fill = Actividad, text = texto, group = Actividad) +
      geom_area() + 
      scale_x_continuous(name = "Año", 
                         breaks = ejex(),
                         limits = range(ejex()), 
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
      tit = "PIB por el enfoque de la producción", 
      subtit = "Valor ajustado por inflación y poder de compra",
      posx = 2008
    )
  
    salida_ply
  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_PIB_Area.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
