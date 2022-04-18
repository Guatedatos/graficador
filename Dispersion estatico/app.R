library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(countrycode)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
original <- read_excel("PIBcap e IDH.xlsx") %>% 
  select(-1) %>% 
  filter(.[[2]] == 2016)

datos <- original %>% 
  select(Pais = 1, IDH, PIBcap) %>% 
  mutate(Continente = countrycode(sourcevar = Pais, 
                                  origin = "country.name", 
                                  destination = "continent"),
         Continente = ifelse(Pais == "Guatemala", "Guatemala", Continente),
         texto = paste0("<b>País:</b> ", Pais, "<br>",
                        "<b>Continente:</b> ", Continente, "<br>",
                        "<b>PIB:</b> ", formato2(PIBcap), "<br>",
                        "<b>IDH:</b> ", formato2(IDH)))

Encoding(datos$texto) <- "UTF-8"

#### INTERFAZ ####
interfaz <- fluidPage(
  title = "Guatemala en Datos",
  plotlyOutput(outputId = "scatter"),
  div(style = "height:25px"),
  splitLayout(
    cellWidths = c("75%", "25%"),
    materialSwitch(inputId = "id_tipo", label = "PIB en logaritmo", status = "success"),
    downloadButton(outputId = "id_descarga", label = "Descargar Datos")
  )
)

#### SERVER ####
servidor <- function(input, output) {
  
  #Escala eje x
  botoncito <- reactive({input$id_tipo})
  
  #Nombre eje x
  titX <- reactive({
    if (botoncito()) {
      "Log PIB per cápita"
    } else {
      "PIB per cápita"
    }
  })
  
  #variable a graficar en el eje X
  datos_re <- reactive({
    if (botoncito()) {
      mutate(datos, X = log(PIBcap))
    } else {
      mutate(datos, X = PIBcap)
    }
  })
  
  ejey <- seq(0, 1, 0.1)
  
  output$scatter <- renderPlotly({

    ejex <- Eje_OK(x = datos_re()$X, 
                   saltos = c(0.1, 0.5, 1:5, 10, 50, 100, 500, 1000, 5000, 10000), 
                   cero = FALSE,
                   optimo = 10)
    
    
    datos_v2 <- datos_re() %>% 
      mutate(
        txty = 0.1,
        txtx = mean(ejex)
      )
    
      z <- ggplot(data = datos_v2) +
        aes(x = X, y = IDH, text = texto, color = Continente) +
        geom_point(alpha = 0.5, size = 1.5) +
        scale_x_continuous(name = titX(), 
                           breaks = ejex, 
                           limits = range(ejex), 
                           labels = formato(ejex),
                           expand = c(0.01, 0.01)) +
        scale_y_continuous(name = "Índice de Desarrollo Humano", 
                           breaks = ejey, 
                           limits = range(ejey), 
                           labels = formato(ejey),
                           expand = c(0.01, 0.01)) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        )

      salida_ply <- estetica(
        grafico = z, 
        tit = "PIB Total vs. IDH - Año 2016", 
        subtit = "Valor ajustado por inflación y poder de compra",
        posx = quantile(ejex, 0.1)
      )

    salida_ply
    
  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_PIB_IDH.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
