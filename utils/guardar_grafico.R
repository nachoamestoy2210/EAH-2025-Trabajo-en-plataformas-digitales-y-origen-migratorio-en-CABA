# Guarda un gráfico como png en output/graficos/ (w y h en pulgadas)
guardar_grafico <- function(g, nombre, w, h) {
  ggsave(file.path(CARPETA_GRAFICOS, paste0(nombre, ".png")), g,
         width = w, height = h, dpi = 150, bg = "white")
}
