library(shinyWidgets)
library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(stringr)
source("fuente.R", encoding = "UTF-8")

#problema: no encontre manera de cambiar las palabras "root", "parent" y "of" en la salida

#### DATOS ####
data2020 <- read_excel("Data 2020.xlsx") %>% 
  select(Unidad, Institución, valor = P2021F) %>% 
  #filter(!is.na(P2020)) %>% 
  #filter(P2020 < quantile(P2020, 0.99))
  filter(
    !is.na(valor),
    Unidad != "Servicios de la Deuda Pública"
  ) %>% 
  group_by(Unidad, Institución) %>% 
  summarise(valor = round(sum(valor/1000000), 2)) %>% 
  ungroup()

original <- data2020 %>% 
  select(Institución, Unidad, Valor = valor) %>% 
  arrange(Institución, Unidad)

datos <- data2020 %>% 
  group_by(Institución) %>% 
  summarise(valor = sum(valor)) %>% 
  rename(Unidad = Institución) %>% 
  mutate(Institución = "Presupuesto Total 2021") %>% 
  bind_rows(data2020) %>% 
  add_count(Unidad) %>% 
  mutate(
    Unidad = ifelse(n > 1, paste0(Unidad, "_", Institución), Unidad),
    
    #Ministerios en rojo, el resto en verde
    colores = ifelse(
      str_detect(Institución, "^Minist") | str_detect(Unidad, "^Minist"), 
      "red", "green")
    )

#### INTERFAZ ####
interfaz <- fluidPage(
  title = "Guatemala en Datos",
  h1(strong("Presupuesto Final 2021"), style = "font-size:22px;"),
  plotlyOutput(outputId = "arbol"),
  div(style = "height:15px"),
  
  splitLayout(
    cellWidths = c("80%", "20%"),
    p("UVG/GuateenDatos/EstadodeDerecho-CCBY", style = "font-size:12px;"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
    )
)

#### SERVER ####
servidor <- function(input, output) {
  
  output$arbol <- renderPlotly({
    
    #grafico <- 
    plot_ly(
      type = 'treemap',
      labels = datos$Unidad,
      parents = datos$Institución,
      values = datos$valor,
      hoverinfo = "text+label+value+percent parent+percent root",
      textinfo = "label+value+percent parent+percent root",
      marker = list(colors = datos$colores)
    ) %>% 
      add_trace(branchvalues = "total", name = "")

    # htmlwidgets::saveWidget(grafico, "plotly.html", 
    #                         selfcontained = TRUE, title = "P2021F en Millones")
    
  })

  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_Treemap.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
