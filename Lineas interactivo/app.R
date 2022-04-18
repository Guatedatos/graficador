library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
original <- read_excel("PIB total long.xlsx")

datos <- original %>% 
    select(Pais = 1, Anio = 2, Valor) %>% 
    mutate(
        Valor = Valor/1000,
        texto = paste0("<b>País:</b> ", Pais, "<br>",
                        "<b>Año:</b> ", Anio, "<br>",
                        "<b>PIB:</b> ", formato2(Valor))
        ) %>% 
    arrange(Pais, Anio)

Encoding(datos$texto) <- "UTF-8"
paises <- sort(unique(datos$Pais))

vacio <- ggplot() +
  aes(x = 0, y = 0, 
      label = "No hay datos para graficar.\nSeleccione al menos un país.") +
  geom_text(size = 5) +
  theme_void()

relativo <- datos %>% 
    group_by(Pais) %>% 
    mutate(rezago = lag(Valor)) %>% 
    ungroup() %>% 
    mutate(
        Valor = 100 * (Valor/rezago - 1),
        Valor = ifelse(is.infinite(Valor), NaN, Valor),
        texto = paste0("<b>País:</b> ", Pais, "<br>",
                       "<b>Año:</b> ", Anio, "<br>",
                       "<b>Cambio Relativo:</b> ", round(Valor, 2), "%")
        )

#### INTERFAZ ####
interfaz <- fluidPage(
    
    title = "Guatemala en Datos",
  
    dropdownButton(
      multiInput(
        inputId = "widget_pais",
        label = "Elegir Países", 
        selected = "Guatemala",
        choices = paises,
        width = "475px"
        ),
      circle = TRUE, status = "success", size = "sm", 
      icon = icon("globe-americas"), width = "500px",
      tooltip = tooltipOptions(title = "Click para elegir países")
    ),
    
    plotlyOutput(outputId = "linea"),
    div(style = "height:25px"),
    
    splitLayout(
      cellWidths = c("75%", "25%"),
      materialSwitch(inputId = "id_tipo", label = "Ver Tasa", status = "success"),
      downloadButton(outputId = "id_descarga", label = "Descargar Datos")
      )
)

#### SERVER ####
servidor <- function(input, output) {
    
    #Tipo de Grafico a visualizar
    botoncito <- reactive({input$id_tipo})
    
    #Conjunto de datos filtrado segun pais
    filtrado <- reactive({
        if (botoncito()) {
            filter(relativo, Pais %in% input$widget_pais)
            } else {
                filter(datos, Pais %in% input$widget_pais)
                }
        })
    
    #Nombre Eje Y
    titY <- reactive({
      if (botoncito()) {
        "Tasa de Crecimiento"
        } else {
          "Miles de millones de dólares de 2011"
          }
    })
    
    output$linea <- renderPlotly({

      if (nrow(filtrado()) == 0) {
          
          salida_ply <- estetica(
            grafico = vacio, 
            tit = "PIB Total", 
            subtit = "Valor ajustado por inflación y poder de compra",
            posx = 0
          )
          
          } else {

        ejex <- Eje_OK(x = filtrado()$Anio, saltos = 20, cero = FALSE)
        ejey <- Eje_OK(
          x = filtrado()$Valor, 
          saltos = c(seq(0.1, 0.9, 0.1), 1:4, seq(5, 50, 5), 
                     seq(100, 500, 50), seq(1000, 16000, 1000)), 
          cero = !botoncito()
          )

        z <- ggplot(data = filtrado()) +
            aes(x = Anio, y = Valor, color = Pais, text = texto, group = Pais) +
            geom_line() +
            scale_x_continuous(name = "Año", 
                               breaks = ejex, 
                               limits = range(ejex), 
                               expand = c(0.01, 0.01)) +
            scale_y_continuous(name = titY(), 
                               breaks = ejey, 
                               labels = formato(ejey),
                               limits = range(ejey), 
                               expand = c(0.01, 0.01)) +
            scale_color_brewer(palette = "Dark2") +
            theme_bw() +
            theme(
                plot.title = element_text(face = "bold"),
                axis.title = element_text(face = "bold"),
                legend.title = element_blank()
            )

        salida_ply <- estetica(
          grafico = z, 
          tit = "PIB Total", 
          subtit = "Valor ajustado por inflación y poder de compra",
          posx = 1830
          )
          }
      
        salida_ply
    })

    output$id_descarga <- downloadHandler(
        filename = "Guate_en_Datos_PIB.csv",
        content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
    )
    
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
