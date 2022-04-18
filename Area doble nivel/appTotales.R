library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
original <- read_excel("Import. totales.xlsx")

datos <- original %>% 
  pivot_longer(-1, names_to = "Anio", values_to = "Valor") %>% 
  rename("Producto" = 1) %>% 
  filter(!is.na(Valor), !Producto %in% c("Total", "PIB")) %>% 
  arrange(desc(Anio), Valor) %>% 
  mutate(
    Valor = Valor/1000,
    Anio = as.numeric(str_remove_all(Anio, "[^[:digit:]]")),
    texto = paste0("<b>Producto:</b> ", Producto, "<br>",
                   "<b>Año:</b> ", Anio, "<br>",
                   "<b>Valor:</b> ", formato2(Valor) #" &times; 10<sup>9</sup>"
                   ),
    Producto = str_wrap(Producto, 15),
    Producto = factor(Producto, levels = unique(Producto))
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
      select(datos, Valor = Pje, Producto, Anio, texto, Total)
    } else {
      datos
    }
  })
  
  #Nombre Eje Y
  titY <- reactive({
    if (botoncito()) {
      "Porcentaje"
      } else {
        "Miles de millones de dólares corrientes CIF"
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
        cero = !botoncito(),
        optimo = 8
        )
    }
  })
  
  #Saltos Eje X
  ejex <- reactive({Eje_OK(x = datos_re()$Anio, saltos = 2, cero = FALSE)})
  
  output$area <- renderPlotly({
  
    z <- ggplot(data = datos_re()) +
      aes(x = Anio, y = Valor, fill = Producto, text = texto, group = Producto) +
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
      tit = "Importaciones Totales", 
      subtit = "Miles de millones de dólares corrientes CIF",
      posx = 1984
    )
  
    salida_ply
  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_Total_Import.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
