# Límites de valores extremos según el rango intercuartílico (cuartiles ponderados)
limites_iqr <- function(x, w, k = 1.5) {
  q <- c(cuantil_ponderado(x, w, 0.25), cuantil_ponderado(x, w, 0.75))
  c(q1 = q[1], q3 = q[2], inf = q[1] - k * diff(q), sup = q[2] + k * diff(q))
}
