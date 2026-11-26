# Intensidad de la asociación según V de Cramér (umbrales de Cohen con 1 gl: .1 / .3 / .5)
magnitud_v <- function(v) {
  cut(v, c(-Inf, 0.1, 0.3, 0.5, Inf), right = FALSE,
      labels = c("Muy débil", "Débil", "Moderada", "Fuerte"))
}
