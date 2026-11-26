# [NUEVO] Agregado desde gráficos-previos.R (usado por script/07_descriptivo_previo.R)
# Coeficiente de asimetría ponderado (ignora NA)
asimetria_pond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  m <- weighted.mean(x, w)
  sum(w * ((x - m) / desvio_pond(x, w))^3) / sum(w)
}
