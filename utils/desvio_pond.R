# Desvío estándar ponderado (ignora NA)
desvio_pond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  sqrt(sum(w * (x - weighted.mean(x, w))^2) / sum(w))
}
