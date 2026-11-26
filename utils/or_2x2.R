# Odds ratio de una tabla 2x2 con IC95% de Wald (filas: grupo / referencia;
# columnas: evento / no evento). Si hay ceros se suma 0.5 a todas las celdas.
or_2x2 <- function(tab) {
  x <- as.numeric(tab)                      # a, c, b, d (por columnas)
  if (any(x == 0)) x <- x + 0.5
  a <- x[1]; c_ <- x[2]; b <- x[3]; d_ <- x[4]
  or <- (a * d_) / (b * c_)
  se <- sqrt(1 / a + 1 / b + 1 / c_ + 1 / d_)
  tibble(or = or, or_inf = exp(log(or) - qnorm(0.975) * se), or_sup = exp(log(or) + qnorm(0.975) * se),
         se_log = se)
}
