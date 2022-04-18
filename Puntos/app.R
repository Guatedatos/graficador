library(shinyWidgets)
library(plotly)
library(shiny)
library(readxl)
library(tidyverse)
source("fuente.R", encoding = "UTF-8")

#### DATOS ####
original <- read_excel("PIBcap e IDH.xlsx") %>% 
  select(-1) %>% 
  filter(.[[2]] >= 1990)

datos <- original %>% 
  select(Pais = 1, Anio = 2, IDH, PIBcap) %>% 
  complete(Pais, Anio) %>% 
  mutate(Guate = Pais == "Guatemala",
         texto = paste0("<b>País:</b> ", Pais, "<br>",
                        "<b>Año:</b> ", Anio, "<br>",
                        "<b>PIB:</b> ", formato2(PIBcap), "<br>",
                        "<b>IDH:</b> ", formato2(IDH)))

datos <- bind_rows(mutate(datos, Anio = 0), datos)
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
        txtx = mean(ejex),
        txtz = paste0("Año ", Anio)
      )
    
      z <- ggplot(data = datos_v2) +
        aes(x = X, y = IDH, color = Guate, text = texto, 
            frame = Anio, shape = Guate) +
        geom_point(alpha = 0.5, size = 1.5) +
        geom_text(aes(x = txtx, y = txty, label = txtz), color = "grey", size = 8) +
        #geom_point(aes(x = 54000, y = txty, shape = "triangle")) +
        #geom_text(aes(x = 55000, y = txty, label = "Guatemala"), size = 3) +
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
        #scale_color_manual(values = c("#A4A4A4", "#0CB7F2")) +
        scale_color_manual(values = c("#01DF01", "black")) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none"
        )

      salida_ply <- estetica(
        grafico = z, 
        tit = "PIB Total vs. IDH", 
        subtit = "Valor ajustado por inflación y poder de compra",
        posx = quantile(ejex, 0.1), 
        posy = -0.25
      ) %>% 
        animation_opts(frame = 500, easing = "linear", redraw = FALSE) %>% 
        animation_slider(currentvalue = list(font = list(color = "white")))

    salida_ply %>% layout(
      annotations = list(
        text = "&#9650; Guatemala",
        showarrow = FALSE,
        x = quantile(ejex, 0.95),
        y = -0.25, #tocar esto para mover la fuente
        yref = "paper",
        align = "left",
        font = list(size = 12)
      )
    )
    
  })
  
  output$id_descarga <- downloadHandler(
    filename = "Guate_en_Datos_PIB_IDH.csv",
    content = function(file) {write.csv(x = original, file = file, row.names = FALSE)}
  )
  
}

#### DEPLOY ####
shinyApp(ui = interfaz, server = servidor)
