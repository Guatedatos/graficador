library(ggplot2)
library(base64enc)
logo <- dataURI(file = "logoV.png")

#ver por que no funciona la f para descargas
#descarga <- function(file) {write.csv(x = original, file = file, row.names = FALSE)}

Eje_OK <- function(x, saltos, cero, optimo = 5) {
  
  #x: vector numerico (datos)
  #saltos: vector numerico, saltos a considerar
  #cero: TRUE implica que el eje arranca si o si en cero
  #optimo: nro deseado de cortes para el eje Y
  
  #saltos <- c(1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500)
  n <- length(saltos)
  vector <- numeric(n)
  ymin <- ifelse(cero, 0, min(x, na.rm = TRUE))
  ymax <- max(x, na.rm = TRUE)      
  for (j in 1:n) {vector[j] <- length(seq(ymin, ymax, by = saltos[j]))}
  k <- saltos[which.min(abs(vector - optimo))] 
  ymin <- k * floor(ymin/k)
  ymax <- k * ceiling(ymax/k)
  
  return(seq(ymin, ymax, by = k))
}
formato <- function(x) {format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)}
formato2 <- function(x) {
  trimws(format(round(x, 2), big.mark = ",", decimal.mark = ".", 
                scientific = FALSE, nsmall = 2))
  }
estetica <- function(grafico, tit, subtit, posx, posy = -0.2) {
  
  ggplotly(grafico, tooltip = "texto") %>%
    # config(modeBarButtonsToRemove = c("pan2d", "zoom2d",
    #                                   "toggleSpikelines", "resetScale2d")) %>%
    layout(
      title = list(
        #tocar esto para cambiar posicion del titulo
        x = 0.05, 
        y = 0.93,
        text = paste0("<b>", tit, "</b><br><sup>", subtit, "</sup>")
      ),
      
      #en plotly NO EXISTEN los subtitulos, por eso es dificil manejar
      #la posicion. En este caso todo lo que figura es parte del titulo
      #principal, con la diferencia de que el subtitulo esta como superscript
      #el espaciado entre lineas es el resultado de poner el salto <br>
      #otra opcion es agregar el subtitulo como annotation (igual que la fuente)
      #pero es mas complejo porque las posiciones dejan de ser relativas
      #(si cambia el eje, hay que cambiar la posicion en las annotations)
      
      font = list(family = "Helvetica"),
      annotations = list(
        text = paste0(
          "Fuente: Gapminder(2020)<br>",
          "UVG/GuateenDatos/ProductoInternoBruto(PIB)-CCBY"
        ),
        showarrow = FALSE,
        x = posx,
        y = posy, #tocar esto para mover la fuente
        yref = "paper",
        align = "left",
        font = list(size = 9)
      ),
      margin = list(t = 60, b = 60),
      images = list(
        source = logo,
        x = 0.01,
        y = 0.99,
        opacity = 0.5,
        sizex = 0.15,
        sizey = 0.15
      )
    )
}

# setwd("C:/Users/diego/Documents/Trabajo/Guatemala")
# carpetas <- list.dirs(recursive = FALSE, full.names = FALSE)
# for (j in carpetas) {file.copy(from = "fuente.R", to = j, overwrite = TRUE)}

