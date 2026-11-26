# Imprime una tabla con título (admite data.frame o tibble)
mostrar <- function(titulo, tabla) {
  cat("\n", titulo, "\n", sep = "")
  print(as_tibble(tabla), n = Inf, width = Inf)
}
