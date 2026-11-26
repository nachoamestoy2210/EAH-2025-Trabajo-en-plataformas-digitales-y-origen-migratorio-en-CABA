# Cuantil, mediana y desvío ponderados (ignoran NA)
cuantil_ponderado <- function(x, w, p = 0.5) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  o <- order(x)
  x[o][which(cumsum(w[o]) >= p * sum(w))[1]]
}
