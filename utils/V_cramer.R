# V de Cramér de una tabla de contingencia
V_cramer <- function(tabla) {
  tabla     <- as.matrix(tabla)
  esperados <- outer(rowSums(tabla), colSums(tabla)) / sum(tabla)
  chi2      <- sum((tabla - esperados)^2 / esperados)
  sqrt(chi2 / (sum(tabla) * (min(dim(tabla)) - 1)))
}
